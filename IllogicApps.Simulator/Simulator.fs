namespace IllogicApps.Simulator

open System
open System.Collections.Generic

open System.Diagnostics
open IllogicApps.Core
open IllogicApps.Core.ExternalServiceTypes
open IllogicApps.Core.LogicAppSpec
open IllogicApps.Expression.Execution
open IllogicApps.Json
open CompletedStepTypes
open IllogicApps.Simulator.ExternalServices
open IllogicApps.Simulator.Parameters

module ExpressionLexer = IllogicApps.Expression.Parsing.Lexer
module ExpressionParser = IllogicApps.Expression.Parsing.Parser

module private SimulatorHelper =
    let private emptyDependencyList = OrderedMap.ofList [ ("", []) ]

    let createDependencyGraph (actions: IReadOnlyDictionary<string, BaseAction>) =
        actions
        |> Seq.collect (fun kv ->
            let name = kv.Key

            kv.Value.RunAfter
            |> Option.defaultValue OrderedMap.empty
            |> (fun m -> if m.Count <> 0 then m else emptyDependencyList)
            |> Seq.map (fun dep -> (dep.Key, name)))
        |> Seq.groupBy fst
        |> Seq.map (fun (k, v) -> (k, v |> Seq.map snd |> List.ofSeq))
        |> Map.ofSeq

    type DependencyStatus =
        | Satisfied
        | Completed
        | Incomplete

    let mergeDependencyStatus acc status =
        match acc, status with
        | Incomplete, _ -> Incomplete
        | _, Incomplete -> Incomplete
        | Completed, _ -> Completed
        | _, Completed -> Completed
        | Satisfied, Satisfied -> Satisfied

    let calculateDependencyStatus (actionResults: IDictionary<string, CompletedAction>) (action: BaseAction) =
        let calcSingleDependency dep requiredStatuses =
            match actionResults.TryGetValue(dep) with
            | true, actionResult ->
                if Seq.contains actionResult.status requiredStatuses then
                    Satisfied
                else
                    Completed
            | _ -> Incomplete

        action.RunAfter
        |> Option.defaultValue OrderedMap.empty
        |> Seq.fold
            (fun acc kv ->
                let requiredStatuses = kv.Value
                let depStatus = calcSingleDependency kv.Key requiredStatuses
                mergeDependencyStatus acc depStatus)
            Satisfied

    let trimLeaves
        (actions: IReadOnlyDictionary<string, BaseAction>)
        (actionResults: IDictionary<string, CompletedAction>)
        (leafActions: string list)
        =
        let rec loop leafActions =
            let changed, newLeaves =
                leafActions
                |> List.fold
                    (fun (chg, acc) name ->
                        let result = actionResults.[name]

                        match result.status with
                        | Skipped ->
                            let higherActions =
                                actions.[name].RunAfter
                                |> Option.defaultValue OrderedMap.empty
                                |> OrderedMap.keys
                                |> List.ofSeq

                            true, (higherActions @ acc)
                        | _ -> chg, name :: acc)
                    (false, [])

            if changed then loop newLeaves else newLeaves

        loop leafActions

    let mergeStatus overall next =
        match (overall, next) with
        | first, Succeeded -> first
        | first, Cancelled -> first
        | first, Skipped -> first
        | Failed, _ -> Failed
        | _, Failed -> Failed
        | TimedOut, _ -> TimedOut
        | _, TimedOut -> TimedOut

    let unpackObject (node: JsonTree) =
        node |> Conversions.ensureObject |> OrderedMap.toSeq

    let arrayOfObjects (node: JsonTree) =
        node |> Conversions.ensureArray |> Seq.map unpackObject

    let rec jsonMapStrs (f: string -> JsonTree) (node: JsonTree) =
        match node with
        | Object o ->
            o
            // TODO: what is the actual LogicApps behaviour for duplicate keys?
            |> OrderedMap.map (fun k v -> (f k |> Conversions.ensureString), jsonMapStrs f v)
            |> Object
        | Array a -> a |> Seq.map (jsonMapStrs f) |> Conversions.createArray
        | String s -> f s
        | _ -> node

    let evaluateLanguageSandboxedForParameter =
        Evaluator.evaluateSandboxed (OrderedMap.ofList [ "appsetting", BuiltinFunctions.f_appsetting ]) OrderedMap.empty

    let evaluateParameter sim v =
        jsonMapStrs (Evaluator.altEvaluateIfNecessary evaluateLanguageSandboxedForParameter sim) v.value

    let logActionPreRun workflowTag actionName actionType =
        printfn "[Action %s:%s (%s) started]" workflowTag actionName actionType

    let logActionPostRun workflowTag actionName actionType result =
        match result.error with
        | None -> printfn "[Action %s:%s (%s) ended: %O]" workflowTag actionName actionType result.status
        | Some err -> printfn "[Action %s:%s (%s) ended: %O (%O)]" workflowTag actionName actionType result.status err

    let skippedResult =
        { ActionResult.Default with
            status = Skipped
            code = Some ActionSkipped
            error =
                Some
                    { code = ActionDependencyFailed
                      message = "Action skipped due to dependency failure" } }

    let skippedBranchResult =
        { ActionResult.Default with
            status = Skipped
            code = Some ActionSkipped
            error =
                Some
                    { code = ActionBranchingConditionNotSatisfied
                      message = "Action skipped as part of a branch not executed" } }

    let skippedTerminatedResult =
        { ActionResult.Default with
            status = Skipped
            code = Some Terminated }

open SimulatorHelper

type private ArrayOperationContextImpl(values: JsonTree list, disposeHook: ArrayOperationContextImpl -> unit) =
    let mutable values = values
    let mutable started = false

    interface ArrayOperationContext with
        member this.Dispose() = disposeHook this

        member this.Advance() =
            if not started then
                started <- true
            else
                values <- values.Tail

            values.IsEmpty |> not

        member this.Current =
            if not started then
                failwith "ArrayOperationContext not advanced before accessing Current"

            values.Head

type TriggerCompletion =
    | Completed of CompletedAction
    | Invoked of HttpRequest

module Simulator =
    let workflowIsStateless (workflow: LogicAppSpec.Root) =
        workflow.kind.Equals("stateless", StringComparison.OrdinalIgnoreCase)

    let stringOfTerminationStatus (status: Result<Status, Status * TerminateRunError option>) =
        match status with
        | Ok result -> $"Completed, {result}"
        | Error(status, None) -> $"Terminated, {status}"
        | Error(status, Some error) -> $"Terminated, {status}: {error}"

[<Struct>]
type SimulatorCreationOptions =
    { workflowName: string
      workflowId: string
      workflowVersion: string
      runId: string
      originatingRunId: string
      triggerResult: TriggerCompletion
      externalServiceHandlers: ExternalServiceHandler list
      isBugForBugAccurate: bool
      isStateless: bool
      isNondeterministic: bool
      appConfig: OrderedMap<string, string>
      parameters: OrderedMap<string, Parameter>
      evaluate: SimulatorContext -> ExpressionParser.Ast -> JsonTree }

    [<CompiledName("Dummy")>]
    static member dummy =
        { workflowName = "unnamed_workflow"
          workflowId = "dummy_workflow_id"
          workflowVersion = "01234567890123456789"
          runId = ""
          originatingRunId = ""
          triggerResult = Completed(CompletedAction.create "" "")
          externalServiceHandlers = []
          isBugForBugAccurate = true
          isStateless = false
          isNondeterministic = false
          appConfig = OrderedMap.empty
          parameters = OrderedMap.empty
          evaluate = Evaluator.evaluate }

    [<CompiledName("CreateSimple")>]
    static member createSimple (logicApp: LogicAppSpec.Root) httpRequest =
        let runId = Guid.NewGuid().ToString()

        { SimulatorCreationOptions.dummy with
            runId = runId
            originatingRunId = runId
            isStateless = Simulator.workflowIsStateless logicApp
            triggerResult = Invoked httpRequest
            externalServiceHandlers = [ loggingHandler; noOpHandler ] }

type private ScopeContextImpl
    (
        actionName: string,
        isRepeating: bool,
        precedingRepetitions: ActionRepetition list,
        potentialChildren: (string * BaseAction) seq,
        simulator: Simulator,
        disposeHook: ScopeContextImpl -> unit
    ) =
    let mutable overallResult = Succeeded
    let mutable notYetRunChildren = Map.ofSeq potentialChildren
    let mutable currentIteration = 0
    let actionResults = MutableOrderedMap<string, CompletedAction>()

    let mergeResult result =
        overallResult <- mergeStatus overallResult result

    let currentRepetitionStack () =
        if isRepeating then
            { loopName = actionName
              repetitionCount = currentIteration }
            :: precedingRepetitions
        else
            precedingRepetitions

    let recordResultOf name (action: BaseAction) (f: unit -> ActionResult) =
        let startTime = DateTime.UtcNow
        let result = simulator.ExecuteAction startTime name action f
        actionResults.[name] <- result
        simulator.RecordActionResult name result
        simulator.RecordActionRepetition (currentRepetitionStack ()) name result
        result

    member this.ActionResults = actionResults

    member this.CurrentRepetitionStack = currentRepetitionStack ()

    member this.ExecuteGraph(actions: ActionGraph) =
        let dependencyGraph = createDependencyGraph actions
        let remainingActions = Dictionary<string, BaseAction>(actions)

        let getNextActions name =
            Map.tryFind name dependencyGraph |> Option.defaultValue []

        let pickRandom list =
            let index = Random.Shared.Next(0, List.length list)
            // let head, item :: tail = List.splitAt index list
            match List.splitAt index list with
            | head, item :: tail -> item :: head @ tail
            | _, [] -> failwith "pickRandom: exceeded list length"

        let maybePickRandom lst =
            if simulator.IsNondeterministic then
                match lst with
                | _ :: _ :: _ -> pickRandom lst
                | _ -> lst
            else
                lst

        let rec executeNext actionQueue =
            match maybePickRandom actionQueue with
            | [] -> ()
            | actionName :: rest ->
                match remainingActions.TryGetValue actionName with
                | false, _ -> executeNext rest
                | true, action ->
                    let status, result =
                        if Result.isError simulator.TerminationStatus then
                            // If we're terminating, just skip everything
                            // (i.e. consider dependencies fulfilled but not in the right state)
                            DependencyStatus.Completed, skippedTerminatedResult
                        else
                            calculateDependencyStatus actionResults action, skippedResult

                    match status with
                    | Satisfied ->
                        remainingActions.Remove actionName |> ignore

                        let actionType = action.ActionType
                        let workflowTag = simulator.WorkflowDetails.name
                        logActionPreRun workflowTag actionName actionType

                        let result =
                            recordResultOf actionName action (fun () -> action.Execute actionName simulator)

                        logActionPostRun workflowTag actionName actionType result

                        rest @ (getNextActions actionName)
                    | DependencyStatus.Completed ->
                        // This action's dependencies are in the wrong state, skip it
                        remainingActions.Remove actionName |> ignore
                        recordResultOf actionName action (fun () -> result) |> ignore
                        action.GetChildren() |> this.ForceSkipAll

                        rest @ (getNextActions actionName)
                    | Incomplete ->
                        // This action's dependencies are not yet complete, try again once something else finishes
                        // (it's ok to not put it back in the queue, as it will be re-added when its next dependency is completed)
                        rest
                    |> executeNext

        executeNext (getNextActions "")

        if Result.isError simulator.TerminationStatus then
            Cancelled
        else
            dependencyGraph.Keys
            |> Set.ofSeq
            |> Set.difference (actions.Keys |> Set.ofSeq)
            |> Set.toList
            |> trimLeaves actions actionResults
            |> Seq.map (fun name -> actionResults.[name].status)
            |> Seq.fold mergeStatus Succeeded

    member this.ForceSkipAll actions =
        actions
        |> Seq.toList
        |> BaseAction.getAllChildren
        |> List.iter (fun (name, action) -> recordResultOf name action (fun () -> skippedBranchResult) |> ignore)

    interface ScopeContext with
        member this.Run actions =
            let result = this.ExecuteGraph actions
            mergeResult result

            if isRepeating then
                // Also record the result of this scope as a repetition of itself
                let result =
                    // Not sure what to put here, maybe we'll need yet another interface method on BaseTrigger
                    // (but that can wait until we have tests for the accuracy of these results)
                    { CompletedAction.create actionName (stringOfDateTime DateTime.UtcNow) with
                        status = result }

                simulator.RecordActionRepetition (currentRepetitionStack ()) actionName result

            notYetRunChildren <- notYetRunChildren |> Map.filter (fun k _ -> not (actions.ContainsKey k))
            currentIteration <- currentIteration + 1
            result

        member this.MergeResult result = mergeResult result
        member this.OverallResult = overallResult

        member this.Dispose() =
            this.ForceSkipAll(Map.toSeq notYetRunChildren)
            disposeHook this

and Simulator private (creationOptions: SimulatorCreationOptions) as this =
    let isBugForBugAccurate = creationOptions.isBugForBugAccurate

    let mutable triggerResult = creationOptions.triggerResult

    let executeAction startTime name (action: BaseAction) (f: unit -> ActionResult) =
        let result =
            try
                f ()
            with e ->
                { ActionResult.Default with
                    status = Failed
                    code = Some ActionFailed
                    error =
                        Some
                            { code = ActionFailed
                              message = $"Action threw exception: {e}" } }

        let trackedProperties =
            match result.status with
            | Skipped -> None
            | _ when creationOptions.isStateless -> None
            | _ ->
                action.TrackedProperties
                |> Object
                |> this.EvaluateLanguage
                |> Conversions.ensureObject
                |> fun v -> if v.Count = 0 then None else Some v

        { CompletedAction.create name (stringOfDateTime startTime) with
            status = result.status
            inputs = result.inputs
            outputs = result.outputs
            trackedProperties = trackedProperties
            error = result.error
            code = result.code
            clientTrackingId = this.TriggerResult.clientTrackingId }

    let executeTrigger name (trigger: BaseTrigger) (request: HttpRequest) =
        let startTime = DateTime.UtcNow

        let originalTriggerResult = triggerResult

        try
            // Set a dummy value for the trigger result, so that we have an initial clientTrackingId/inputs/etc
            triggerResult <-
                Completed
                    { CompletedAction.create name (stringOfDateTime startTime) with
                        inputs = trigger.ProcessInputs this
                        endTime = ""
                        clientTrackingId = creationOptions.runId
                        originHistoryName = Some creationOptions.originatingRunId }

            let result =
                executeAction startTime name trigger (fun () -> trigger.RunFromRequest request this)

            let result =
                { result with
                    originHistoryName = Some creationOptions.originatingRunId }

            // Set an intermediate result before evaluating the clientTrackingId
            // (so that it can use trigger outputs)
            triggerResult <- Completed result

            let result =
                { result with
                    clientTrackingId =
                        match trigger.ClientTrackingId with
                        | Some code -> this.EvaluateLanguage code |> Conversions.ensureString
                        | None -> creationOptions.runId }

            triggerResult <- Completed result
        with _ ->
            triggerResult <- originalTriggerResult
            reraise ()

    let evaluatedParameters =
        creationOptions.parameters
        |> OrderedMap.mapValuesOnly (fun v -> lazy evaluateParameter this v)

    let variables = Dictionary<string, JsonTree>()
    let canonicalVarCase = Dictionary<string, string>()

    let canonicalActionResultCase = Dictionary<string, string>()

    let arrayOperationContextStack = Stack<string option * ArrayOperationContextImpl>()
    let scopeContextStack = Stack<ScopeContextImpl>()

    static member CreateUntriggered(creationOptions: SimulatorCreationOptions) = Simulator(creationOptions)

    member this.CompleteWith result =
        if this.TerminationStatus = Ok Skipped then
            this.TerminationStatus <- Ok result

        Debug.Assert(arrayOperationContextStack.Count = 0)

    member this.ExecuteTrigger name trigger =
        match triggerResult with
        | Completed _ -> failwith "Trigger already executed"
        | Invoked request -> executeTrigger name trigger request

    member this.EditTrigger f = triggerResult <- f triggerResult

    member this.RunWholeWorkflow(actions: ActionGraph) =
        use workflowScope = this.PushScopeContext "" false (OrderedMap.toSeq actions)
        let result = workflowScope.Run actions
        this.CompleteWith result

    static member Trigger
        (trigger: string * BaseTrigger)
        (actions: ActionGraph)
        (creationOptions: SimulatorCreationOptions)
        =
        let sim = Simulator.CreateUntriggered(creationOptions)

        match creationOptions.triggerResult with
        | Completed _ -> ()
        | _ -> sim.ExecuteTrigger (fst trigger) (snd trigger)

        sim.RunWholeWorkflow actions
        sim

    static member TriggerSimple (logicApp: LogicAppSpec.Root) triggerOutputs =
        Simulator.Trigger
            (logicApp.definition.triggers |> OrderedMap.toSeq |> Seq.head)
            logicApp.definition.actions
            { SimulatorCreationOptions.createSimple logicApp triggerOutputs with
                isStateless = Simulator.workflowIsStateless logicApp }

    member val TerminationStatus: Result<Status, Status * TerminateRunError option> = Ok Skipped with get, set
    member val ActionResults = MutableOrderedMap<string, CompletedAction>() with get
    member val ActionIterations = MutableOrderedMap<string, (ActionRepetition list * CompletedAction) list>() with get
    member this.Variables = variables

    member this.GetVariable(name: string) =
        match canonicalVarCase.TryGetValue(name.ToLowerInvariant()) with
        | true, key -> Some variables.[key]
        | _ -> None

    member this.SetVariable name value =
        variables.[name] <- value
        canonicalVarCase.[name.ToLowerInvariant()] <- name

    member this.ArrayOperationContext =
        match arrayOperationContextStack.TryPeek() with
        | true, (_, context) -> Some(context: ArrayOperationContext)
        | _ -> None

    member this.TriggerResult =
        match triggerResult with
        | Invoked _ -> failwith "Trigger result not yet available (invalid order of operations)"
        | Completed completedTrigger -> completedTrigger

    member this.IsBugForBugAccurate = isBugForBugAccurate
    member this.IsNondeterministic = creationOptions.isNondeterministic
    member this.AllActionResults = this.ActionResults |> OrderedMap.CreateRange

    member this.WorkflowDetails: WorkflowDetails =
        WorkflowDetails.create
            creationOptions.workflowId
            creationOptions.workflowName
            creationOptions.workflowVersion
            creationOptions.runId

    member this.GetActionResult(name: string) =
        canonicalActionResultCase.TryGetValue(name.ToLowerInvariant())
        |> function
            | true, result -> Some this.ActionResults.[result]
            | _ -> None

    member internal this.RecordActionResult name result =
        canonicalActionResultCase.[name.ToLowerInvariant()] <- name
        this.ActionResults.[name] <- result

    member internal this.RecordActionRepetition stack name result =
        let previousRepetitions =
            match this.ActionIterations.TryGetValue(name) with
            | true, p -> p
            | _ -> []

        match previousRepetitions with
        | (oldStack, _) :: _ when oldStack.Length > stack.Length ->
            // This is only intended to catch the case where the scope action itself is already recorded via a scope
            // repetition, and then it comes in to get recorded again after fully finishing.
            ()
        | _ -> this.ActionIterations.[name] <- (stack, result) :: previousRepetitions

    member this.GetActionRepetitions name =
        this.ActionIterations.[name]
        |> List.map (fun (stack, result) -> List.rev stack, result)
        |> List.rev

    member this.GetAppConfig name =
        OrderedMap.tryFindCaseInsensitive name creationOptions.appConfig

    member this.GetParameter name =
        OrderedMap.tryFindCaseInsensitive name evaluatedParameters |> Option.map _.Value

    member this.EvaluateCondition expr =
        let rec validate (expr: (string * JsonTree) seq) =
            let func, args = expr |> Seq.exactlyOne

            if
                (OrderedMap.tryFindCaseInsensitive func BuiltinFunctions.functions).IsNone
                || (OrderedMap.tryFindCaseInsensitive func BuiltinFunctions.lazyFunctions).IsNone
            then
                failwithf "Unknown function %s" func

            args
            |> Conversions.ensureArray
            |> Seq.iter (function
                | Object o -> o |> OrderedMap.toSeq |> validate
                | _ -> ())

        let rec transform (expr: (string * JsonTree) seq) : ExpressionParser.Ast =
            let func, args = expr |> Seq.exactlyOne

            let args =
                match args with
                | Array a ->
                    a
                    |> Seq.map (function
                        | Object o -> o |> OrderedMap.toSeq |> transform
                        | String v -> v |> ExpressionLexer.lex |> ExpressionParser.parse
                        | Integer _
                        | Float _
                        | Decimal _
                        | Boolean _
                        | Null as v -> ExpressionParser.Literal(v)
                        | Array _ -> failwith "Array not allowed as argument in condition")
                | Object o -> [ o |> OrderedMap.toSeq |> transform ]
                | _ -> failwith "Condition parameters must be an array or object"

            ExpressionParser.Call(func, args |> List.ofSeq)

        expr
        |> unpackObject
        |> transform
        |> Evaluator.evaluate this
        |> Conversions.ensureBoolean

    member this.EvaluateLanguage expr =
        expr
        |> jsonMapStrs (Evaluator.altEvaluateIfNecessary creationOptions.evaluate this)

    member this.Terminate status error =
        this.TerminationStatus <- Error(status, error)

    member this.ExternalServiceRequest request =
        if not (runAllHandlers creationOptions.externalServiceHandlers this request) then
            failwithf "No handler for external service request: %O" request

    member this.PushArrayOperationContext (actionName: string option) (array: JsonTree seq) : ArrayOperationContext =
        let loopContext =
            new ArrayOperationContextImpl(List.ofSeq array, this.PopAndCompareArrayOperationContext)

        arrayOperationContextStack.Push((actionName, loopContext))
        loopContext

    member private this.PopAndCompareArrayOperationContext context =
        let top = arrayOperationContextStack.Pop()

        if not (LanguagePrimitives.PhysicalEquality (snd top) context) then
            raise <| InvalidOperationException("Loop context push/pop mismatch")

    member this.PushScopeContext
        (actionName: string)
        (isRepeating: bool)
        (potentialActions: (string * BaseAction) seq)
        : ScopeContext =
        let repetitionStack =
            match scopeContextStack.TryPeek() with
            | true, context -> context.CurrentRepetitionStack
            | _ -> []

        let scopeContext =
            new ScopeContextImpl(
                actionName,
                isRepeating,
                repetitionStack,
                potentialActions,
                this,
                this.PopAndFinaliseScopeContext
            )

        scopeContextStack.Push(scopeContext)
        scopeContext

    member private this.PopAndFinaliseScopeContext(context: ScopeContextImpl) =
        let top = scopeContextStack.Pop()

        if not (LanguagePrimitives.PhysicalEquality top context) then
            raise <| InvalidOperationException("Scope context push/pop mismatch")

        // TODO: record whole-scope results
        // TODO: check if `@result` is able to be used on a partially executed scope and/or from within the same scope
        ()

    member internal this.ExecuteAction startTime name action f = executeAction startTime name action f

    member this.GetArrayOperationContextByName(name) =
        arrayOperationContextStack
        |> Seq.tryPick (fun (n, v) -> n |> Option.filter ((=) name) |> Option.map (fun _ -> v: ArrayOperationContext))

    interface SimulatorContext with
        member this.GetVariable name = this.GetVariable name
        member this.SetVariable name value = this.SetVariable name value
        member this.GetAppConfig name = this.GetAppConfig name
        member this.GetParameter name = this.GetParameter name
        member this.IsBugForBugAccurate = this.IsBugForBugAccurate
        member this.ArrayOperationContext = this.ArrayOperationContext
        member this.TriggerResult = this.TriggerResult
        member this.AllActionResults = this.AllActionResults
        member this.WorkflowDetails = this.WorkflowDetails
        member this.GetActionResult name = this.GetActionResult name
        member this.GetActionRepetitions name = this.GetActionRepetitions name
        member this.Terminate status error = this.Terminate status error
        member this.EvaluateCondition expr = this.EvaluateCondition expr
        member this.EvaluateLanguage expr = this.EvaluateLanguage expr
        member this.ExternalServiceRequest request = this.ExternalServiceRequest request

        member this.PushArrayOperationContext actionName array =
            this.PushArrayOperationContext actionName array

        member this.GetArrayOperationContextByName name =
            this.GetArrayOperationContextByName name

        member this.PushScopeContext scopeName isRepeating potentialActions =
            this.PushScopeContext scopeName isRepeating potentialActions
