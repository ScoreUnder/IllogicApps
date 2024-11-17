namespace IllogicApps.Simulator

open System
open System.Collections.Generic

open System.Diagnostics
open IllogicApps.Core
open IllogicApps.Core.ExternalServiceTypes
open IllogicApps.Core.LogicAppSpec
open IllogicApps.Json
open IllogicApps.Simulator.BuiltinCondition
open CompletedStepTypes
open IllogicApps.Simulator.ExternalServices
open IllogicApps.Simulator.Parameters

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
        | first, Skipped -> first
        | Failed, _ -> Failed
        | _, Failed -> Failed
        | TimedOut, _ -> TimedOut
        | _, TimedOut -> TimedOut
        | Succeeded, Succeeded -> Succeeded
        | c -> failwithf "Unexpected status combination %A" c

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
        LanguageEvaluator.evaluateSandboxed
            (OrderedMap.ofList [ "appsetting", BuiltinFunctions.f_appsetting ])
            OrderedMap.empty

    let evaluateParameter sim v =
        jsonMapStrs (LanguageEvaluator.altEvaluateIfNecessary evaluateLanguageSandboxedForParameter sim) v.value

    let logActionPreRun actionName actionType =
        printfn "[Action %s (%s) started]" actionName actionType

    let logActionPostRun actionName actionType result =
        match result.error with
        | None -> printfn "[Action %s (%s) ended: %A]" actionName actionType result.status
        | Some err -> printfn "[Action %s (%s) ended: %A (%A)]" actionName actionType result.status err

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

type private ArrayOperationContextImpl(values: JsonTree list, disposeHook: ArrayOperationContext -> unit) =
    let mutable values = values

    interface ArrayOperationContext with
        member this.Dispose() = disposeHook this

        member this.Advance() =
            values <- values.Tail
            values.IsEmpty |> not

        member this.Current = values.Head

[<Struct>]
type SimulatorCreationOptions =
    { workflowName: string
      triggerResult: CompletedTrigger
      externalServiceHandlers: ExternalServiceHandler list
      isBugForBugAccurate: bool
      appConfig: OrderedMap<string, string>
      parameters: OrderedMap<string, Parameter> }

    static member Default =
        { workflowName = "unnamed_workflow"
          triggerResult = CompletedTrigger.create <| CompletedAction.create "" ""
          externalServiceHandlers = []
          isBugForBugAccurate = true
          appConfig = OrderedMap.empty
          parameters = OrderedMap.empty }

    static member createSimple (logicApp: LogicAppSpec.Root) triggerOutputs =
        { SimulatorCreationOptions.Default with
            triggerResult =
                CompletedTrigger.create
                    { CompletedAction.create
                          (logicApp.definition.triggers.Keys |> Seq.head)
                          (stringOfDateTime DateTime.UtcNow) with
                        outputs = triggerOutputs }
            externalServiceHandlers = [ loggingHandler; noOpHandler ] }

type Simulator private (creationOptions: SimulatorCreationOptions) as this =
    inherit SimulatorContext()

    let isBugForBugAccurate = creationOptions.isBugForBugAccurate

    let recordResultOf name (action: BaseAction) (f: unit -> ActionResult) =
        let startTime = DateTime.UtcNow

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
            | _ ->
                action.TrackedProperties
                |> Object
                |> this.EvaluateLanguage
                |> Conversions.ensureObject
                |> Some

        let result =
            { CompletedAction.create name (stringOfDateTime startTime) with
                status = result.status
                inputs = result.inputs
                outputs = result.outputs
                trackedProperties = trackedProperties
                error = result.error
                code = result.code
                clientTrackingId = creationOptions.triggerResult.action.clientTrackingId }

        this.RecordActionResult name result
        result

    let evaluatedParameters =
        creationOptions.parameters
        |> OrderedMap.mapValuesOnly (fun v -> lazy evaluateParameter this v)

    let variables = Dictionary<string, JsonTree>()

    member val private ArrayOperationContextStack = Stack<ArrayOperationContextImpl>() with get

    static member CreateUntriggered(creationOptions: SimulatorCreationOptions) = Simulator(creationOptions)

    member this.CompleteWith result =
        if this.TerminationStatus = Ok Skipped then
            this.TerminationStatus <- Ok result

        Debug.Assert(this.ArrayOperationContextStack.Count = 0)

    static member Trigger (actions: ActionGraph) (creationOptions: SimulatorCreationOptions) =
        let sim = Simulator.CreateUntriggered(creationOptions)
        let result = sim.ExecuteGraph actions
        sim.CompleteWith result
        sim

    static member TriggerSimple (logicApp: LogicAppSpec.Root) triggerOutputs =
        Simulator.Trigger logicApp.definition.actions (SimulatorCreationOptions.createSimple logicApp triggerOutputs)

    member val TerminationStatus: Result<Status, Status * TerminateRunError option> = Ok Skipped with get, set
    member val ActionResults = MutableOrderedMap<string, CompletedAction>() with get
    member this.Variables = variables

    override this.GetVariable name =
        match variables.TryGetValue name with
        | true, value -> Some value
        | _ -> None

    override this.SetVariable name value = variables.[name] <- value
    override this.ArrayOperationContext = this.ArrayOperationContextStack.Peek()
    override this.TriggerResult = creationOptions.triggerResult
    override this.IsBugForBugAccurate = isBugForBugAccurate
    override this.AllActionResults = this.ActionResults |> OrderedMap.CreateRange

    override this.WorkflowDetails =
        WorkflowDetails.Create
            $"dummyWorkflowId_{creationOptions.workflowName}"
            creationOptions.workflowName
            creationOptions.triggerResult.action.clientTrackingId

    override this.GetActionResult name =
        this.ActionResults.TryGetValue name
        |> function
            | true, result -> Some result
            | _ -> None

    member private this.RecordActionResult name result = this.ActionResults.[name] <- result

    override this.GetAppConfig name =
        OrderedMap.tryFindCaseInsensitive name creationOptions.appConfig

    override this.GetParameter name =
        OrderedMap.tryFindCaseInsensitive name evaluatedParameters |> Option.map _.Value

    override this.ExecuteGraph(actions: ActionGraph) =
        let dependencyGraph = createDependencyGraph actions
        let remainingActions = Dictionary<string, BaseAction>(actions)

        let getNextActions name =
            Map.tryFind name dependencyGraph |> Option.defaultValue []

        let rec executeNext actionQueue =
            match actionQueue with
            | [] -> ()
            | actionName :: rest ->
                match remainingActions.TryGetValue actionName with
                | false, _ -> executeNext rest
                | true, action ->
                    let status, result =
                        if Result.isError this.TerminationStatus then
                            // If we're terminating, just skip everything
                            // (i.e. consider dependencies fulfilled but not in the right state)
                            Completed, skippedTerminatedResult
                        else
                            calculateDependencyStatus this.ActionResults action, skippedResult

                    match status with
                    | Satisfied ->
                        remainingActions.Remove actionName |> ignore

                        let actionType = action.ActionType
                        logActionPreRun actionName actionType

                        let result =
                            recordResultOf actionName action (fun () -> action.Execute actionName this)

                        logActionPostRun actionName actionType result

                        rest @ (getNextActions actionName)
                    | Completed ->
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

        if Result.isError this.TerminationStatus then
            Cancelled
        else
            dependencyGraph.Keys
            |> Set.ofSeq
            |> Set.difference (actions.Keys |> Set.ofSeq)
            |> Set.toList
            |> trimLeaves actions this.ActionResults
            |> Seq.map (fun name -> this.ActionResults.[name].status)
            |> Seq.fold mergeStatus Succeeded

    override this.EvaluateCondition expr =
        let rec eval expr =
            let kv = expr |> Seq.exactlyOne
            let key, value = kv

            match key with
            | "and" -> value |> arrayOfObjects |> Seq.forall eval
            | "or" -> value |> arrayOfObjects |> Seq.exists eval
            | "not" -> value |> unpackObject |> eval |> not
            | LanguageCondition fn -> value |> Conversions.ensureArray |> List.ofSeq |> fn
            | _ -> failwithf "Unexpected expression %A" expr

        expr |> unpackObject |> eval

    override this.EvaluateLanguage expr =
        expr |> jsonMapStrs (LanguageEvaluator.evaluateIfNecessary this)

    override this.Terminate status error =
        this.TerminationStatus <- Error(status, error)

    override this.ExternalServiceRequest request =
        if not (runAllHandlers creationOptions.externalServiceHandlers this request) then
            failwithf "No handler for external service request: %A" request

    override this.PushArrayOperationContext(arg1: JsonTree seq) : ArrayOperationContext =
        this.PushLoopContext(this.ArrayOperationContextStack, arg1)

    member private this.PushLoopContext(stack: Stack<ArrayOperationContextImpl>, values: JsonTree seq) =
        let loopContext =
            new ArrayOperationContextImpl(List.ofSeq values, this.PopAndCompareLoopContext stack)

        stack.Push(loopContext)
        loopContext

    member private this.PopAndCompareLoopContext (stack: Stack<ArrayOperationContextImpl>) context =
        let top = stack.Peek(): ArrayOperationContext

        if top = context then
            stack.Pop() |> ignore
        else
            raise <| new InvalidOperationException("Loop context push/pop mismatch")

    override this.ForceSkipAll actions =
        actions
        |> Seq.toList
        |> BaseAction.getAllChildren
        |> List.iter (fun (name, action) -> recordResultOf name action (fun () -> skippedBranchResult) |> ignore)
