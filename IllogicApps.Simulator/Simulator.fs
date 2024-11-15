namespace IllogicApps.Simulator

open System
open System.Collections.Generic

open System.Diagnostics
open IllogicApps.Core
open IllogicApps.Core.ExternalServiceTypes
open IllogicApps.Core.LogicAppBaseAction
open IllogicApps.Json
open IllogicApps.Simulator.BuiltinCondition
open CompletedStepTypes
open IllogicApps.Simulator.ExternalServices
open IllogicApps.Simulator.Parameters

module private SimulatorHelper =
    let private emptyDependencyList = OrderedMap.ofList [ ("", []) ]

    let createDependencyGraph (actions: IReadOnlyDictionary<string, #IGraphExecutable>) =
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

    let calculateDependencyStatus (actionResults: IDictionary<string, CompletedAction>) (action: IGraphExecutable) =
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
        (actions: IReadOnlyDictionary<string, #IGraphExecutable>)
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
        | (first, Skipped) -> first
        | (Failed, _) -> Failed
        | (_, Failed) -> Failed
        | (TimedOut, _) -> TimedOut
        | (_, TimedOut) -> TimedOut
        | (Succeeded, Succeeded) -> Succeeded
        | c -> failwithf "Unexpected status combination %A" c

    let unpackObject (node: JsonTree) =
        match node with
        | Object o -> o |> OrderedMap.toSeq
        | _ -> failwithf "Expected object, got %O" (JsonTree.getType node)

    let unpackArray (node: JsonTree) =
        match node with
        | Array a -> a
        | _ -> failwithf "Expected array, got %O" (JsonTree.getType node)

    let arrayOfObjects (node: JsonTree) =
        match node with
        | Array a -> a |> Seq.map unpackObject
        | _ -> failwithf "Expected array of objects, got %O" node

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

    let getActionType action =
        match box action with
        | :? BaseAction as action -> action.ActionType
        | _ -> action.GetType().Name

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

type private LoopContextImpl(values: JsonTree list, disposeHook: LoopContext -> unit) as this =
    inherit LoopContext()

    [<DefaultValue>]
    val mutable values: JsonTree list

    do this.values <- values

    override this.Dispose() = disposeHook this

    override this.Advance() =
        this.values <- this.values.Tail
        this.values.IsEmpty |> not

    override this.Current = this.values.Head

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

    let recordResultOf name (f: unit -> ActionResult) =
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

        let result =
            { CompletedAction.create name (stringOfDateTime startTime) with
                status = result.status
                inputs = result.inputs
                outputs = result.outputs
                error = result.error
                code = result.code
                clientTrackingId = creationOptions.triggerResult.action.clientTrackingId }

        this.RecordActionResult name result
        result

    let evaluatedParameters =
        creationOptions.parameters
        |> OrderedMap.mapValuesOnly (fun v -> lazy evaluateParameter this v)

    let variables = Dictionary<string, JsonTree>()

    member val private LoopContextStack = Stack<LoopContextImpl>() with get
    member val private ArrayOperationContextStack = Stack<LoopContextImpl>() with get

    static member Trigger (actions: OrderedMap<string, #IGraphExecutable>) (creationOptions: SimulatorCreationOptions) =
        let sim = Simulator(creationOptions)

        let result = sim.ExecuteGraph actions

        if sim.TerminationStatus.IsNone then
            sim.StopExecuting result

        Debug.Assert(sim.LoopContextStack.Count = 0)
        Debug.Assert(sim.ArrayOperationContextStack.Count = 0)

        sim

    static member TriggerSimple (logicApp: LogicAppSpec.Root) triggerOutputs =
        Simulator.Trigger logicApp.definition.actions (SimulatorCreationOptions.createSimple logicApp triggerOutputs)

    member val TerminationStatus: Status option = None with get, set
    member val ActionResults = MutableOrderedMap<string, CompletedAction>() with get
    member this.Variables = variables

    override this.GetVariable name =
        match variables.TryGetValue name with
        | true, value -> Some value
        | _ -> None

    override this.SetVariable name value = variables.[name] <- value
    override this.LoopContext = this.LoopContextStack.Peek()
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
        // TODO are these case sensitive?
        OrderedMap.tryFind name creationOptions.appConfig

    override this.GetParameter name =
        // TODO are these case sensitive?
        OrderedMap.tryFind name evaluatedParameters |> Option.map _.Value

    override this.ExecuteGraph(actions: OrderedMap<string, 'a> when 'a :> IGraphExecutable) =
        let dependencyGraph = createDependencyGraph actions
        let remainingActions = Dictionary<string, 'a>(actions)

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
                        if this.TerminationStatus.IsSome then
                            // If we're terminating, just skip everything
                            // (i.e. consider dependencies fulfilled but not in the right state)
                            Completed, skippedTerminatedResult
                        else
                            calculateDependencyStatus this.ActionResults action, skippedResult

                    match status with
                    | Satisfied ->
                        remainingActions.Remove actionName |> ignore

                        let actionType = getActionType action
                        logActionPreRun actionName actionType
                        let result = recordResultOf actionName (fun () -> action.Execute this)
                        logActionPostRun actionName actionType result

                        rest @ (getNextActions actionName)
                    | Completed ->
                        // This action's dependencies are in the wrong state, skip it
                        remainingActions.Remove actionName |> ignore
                        recordResultOf actionName (fun () -> result) |> ignore
                        action.GetChildren() |> this.ForceSkipAll

                        rest @ (getNextActions actionName)
                    | Incomplete ->
                        // This action's dependencies are not yet complete, try again once something else finishes
                        // (it's ok to not put it back in the queue, as it will be re-added when its next dependency is completed)
                        rest
                    |> executeNext

        executeNext (getNextActions "")

        if this.TerminationStatus.IsSome then
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
            | LanguageCondition fn -> value |> unpackArray |> List.ofSeq |> fn
            | _ -> failwithf "Unexpected expression %A" expr

        expr |> unpackObject |> eval

    override this.EvaluateLanguage expr =
        expr |> jsonMapStrs (LanguageEvaluator.evaluateIfNecessary this)

    override this.StopExecuting status = this.TerminationStatus <- Some status

    override this.ExternalServiceRequest request =
        if not (runAllHandlers creationOptions.externalServiceHandlers this request) then
            failwithf "No handler for external service request: %A" request

    override this.PushLoopContext(arg1: JsonTree seq) : LoopContext =
        this.PushLoopContext(this.LoopContextStack, arg1)

    override this.PushArrayOperationContext(arg1: JsonTree seq) : LoopContext =
        this.PushLoopContext(this.ArrayOperationContextStack, arg1)

    member private this.PushLoopContext(stack: Stack<LoopContextImpl>, values: JsonTree seq) =
        let loopContext =
            new LoopContextImpl(List.ofSeq values, this.PopAndCompareLoopContext stack)

        stack.Push(loopContext)
        loopContext

    member private this.PopAndCompareLoopContext (stack: Stack<LoopContextImpl>) context =
        let top = stack.Peek(): LoopContext

        if top = context then
            stack.Pop() |> ignore
        else
            raise <| new InvalidOperationException("Loop context push/pop mismatch")

    override this.ForceSkipAll actions =
        actions
        |> Seq.map (fun (k, v) -> k, (v: IGraphExecutable))
        |> Seq.toList
        |> getAllChildren
        |> List.iter (fun (name, _) -> recordResultOf name (fun () -> skippedBranchResult) |> ignore)
