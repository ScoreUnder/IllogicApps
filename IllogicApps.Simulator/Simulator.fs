namespace IllogicApps.Simulator

open System
open System.Collections.Generic

open IllogicApps.Core
open IllogicApps.Core.LogicAppBaseAction
open IllogicApps.Json
open IllogicApps.Simulator.BuiltinCondition
open CompletedStepTypes

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

    let skippedResult =
        { status = Skipped
          inputs = None
          outputs = None }

open SimulatorHelper

type LoopContextImpl(values: JsonTree list, disposeHook: LoopContext -> unit) as this =
    inherit LoopContext()

    [<DefaultValue>]
    val mutable values: JsonTree list

    do this.values <- values

    override this.Dispose() = disposeHook this

    override this.Advance() =
        this.values <- this.values.Tail
        this.values.IsEmpty |> not

    override this.Current = this.values.Head

type Simulator private (triggerResult: CompletedTrigger, ?isBugForBugAccurate: bool) as this =
    inherit SimulatorContext()

    let isBugForBugAccurate = defaultArg isBugForBugAccurate true

    let recordResultOf name (f: unit -> ActionResult) =
        let startTime = DateTime.UtcNow
        let result = f ()

        let result =
            { CompletedAction.create name (stringOfDateTime startTime) with
                status = result.status
                inputs = result.inputs
                outputs = result.outputs
                clientTrackingId = triggerResult.action.clientTrackingId }

        this.RecordActionResult name result

    static member Trigger (logicApp: LogicAppSpec.Root) triggerResult =
        let sim = new Simulator(triggerResult)
        let result = sim.ExecuteGraph logicApp.definition.actions

        if sim.TerminationStatus.IsNone then
            sim.StopExecuting result

        sim

    static member TriggerSimple (logicApp: LogicAppSpec.Root) triggerOutputs =
        let triggerResult =
            CompletedTrigger.create
                { CompletedAction.create
                      (logicApp.definition.triggers.Keys |> Seq.head)
                      (stringOfDateTime DateTime.UtcNow) with
                    outputs = triggerOutputs }

        Simulator.Trigger logicApp triggerResult

    member val TerminationStatus: Status option = None with get, set
    member val ActionResults = Dictionary<string, CompletedAction>() with get
    member val LoopContextStack = Stack<LoopContextImpl>() with get
    member val ArrayOperationContextStack = Stack<LoopContextImpl>() with get

    override this.LoopContext = this.LoopContextStack.Peek()
    override this.ArrayOperationContext = this.ArrayOperationContextStack.Peek()
    override this.TriggerResult = triggerResult
    override this.IsBugForBugAccurate = isBugForBugAccurate

    override this.GetActionResult name =
        this.ActionResults.TryGetValue name
        |> function
            | true, result -> Some result
            | _ -> None

    member private this.RecordActionResult name result = this.ActionResults.[name] <- result

    override this.ExecuteGraph(actions: OrderedMap<string, 'a> when 'a :> IGraphExecutable) =
        let dependencyGraph = createDependencyGraph actions
        let remainingActions = new Dictionary<string, 'a>(actions)

        let getNextActions name =
            Map.tryFind name dependencyGraph |> Option.defaultValue []

        let rec executeNext actionQueue =
            match actionQueue with
            | [] -> ()
            | actionName :: rest ->
                match remainingActions.TryGetValue actionName with
                | false, _ -> executeNext rest
                | true, action ->
                    let status =
                        if this.TerminationStatus.IsSome then
                            // If we're terminating, just skip everything
                            // (i.e. consider dependencies fulfilled but not in the right state)
                            Completed
                        else
                            calculateDependencyStatus this.ActionResults action

                    match status with
                    | Satisfied ->
                        remainingActions.Remove actionName |> ignore
                        recordResultOf actionName (fun () -> action.Execute this)

                        rest @ (getNextActions actionName)
                    | Completed ->
                        // This action's dependencies are in the wrong state, skip it
                        remainingActions.Remove actionName |> ignore
                        recordResultOf actionName (fun () -> skippedResult)
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
        printfn "External service request: %A" request
        ()

    override this.PushLoopContext(arg1: JsonTree seq) : LoopContext =
        this.PushLoopContext(this.LoopContextStack, arg1)

    override this.PushArrayOperationContext(arg1: JsonTree seq) : LoopContext =
        this.PushLoopContext(this.ArrayOperationContextStack, arg1)

    member this.PushLoopContext(stack: Stack<LoopContextImpl>, values: JsonTree seq) =
        let loopContext =
            new LoopContextImpl(List.ofSeq values, this.PopAndCompareLoopContext stack)

        stack.Push(loopContext)
        loopContext

    member this.PopAndCompareLoopContext (stack: Stack<LoopContextImpl>) context =
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
        |> List.iter (fun (name, _) -> recordResultOf name (fun () -> skippedResult))
