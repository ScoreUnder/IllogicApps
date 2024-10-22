namespace IllogicApps.Simulator

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Nodes
open IllogicApps.Core
open IllogicApps.Core.LogicAppBaseAction
open IllogicApps.Simulator.BuiltinCondition

module private SimulatorHelper =
    let private emptyDependencyList = Map.ofList [ ("", []) ]

    let createDependencyGraph (actions: IDictionary<string, #IGraphExecutable>) =
        actions
        |> Seq.collect (fun kv ->
            let name = kv.Key

            kv.Value.RunAfter
            |> Option.defaultValue Map.empty
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

    let calculateDependencyStatus (actionResults: IDictionary<string, ActionResult>) (action: IGraphExecutable) =
        let calcSingleDependency dep requiredStatuses =
            match actionResults.TryGetValue(dep) with
            | true, actionResult ->
                if Seq.contains actionResult.status requiredStatuses then
                    Satisfied
                else
                    Completed
            | _ -> Incomplete

        action.RunAfter
        |> Option.defaultValue Map.empty
        |> Seq.fold
            (fun acc kv ->
                let requiredStatuses = kv.Value
                let depStatus = calcSingleDependency kv.Key requiredStatuses
                mergeDependencyStatus acc depStatus)
            Satisfied

    let trimLeaves
        (actions: Map<string, #IGraphExecutable>)
        (actionResults: IDictionary<string, ActionResult>)
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
                                |> Option.defaultValue Map.empty
                                |> Map.keys
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

    let arrayOfObjects (node: JsonNode) =
        match node with
        | :? JsonArray as a -> a |> Seq.map _.AsObject()
        | _ -> failwithf "Expected array of objects, got %O" node

    let rec jsonMapStrs (f: string -> JsonNode) (node: JsonNode) =
        match node.GetValueKind() with
        | JsonValueKind.Undefined -> failwith "Undefined value"
        | JsonValueKind.Object ->
            new JsonObject(
                node.AsObject()
                |> Seq.map (fun kv ->
                    new KeyValuePair<string, JsonNode>((f kv.Key).GetValue<string>(), jsonMapStrs f kv.Value))
            )
            : JsonNode
        | JsonValueKind.Array -> new JsonArray(node.AsArray() |> Seq.map (jsonMapStrs f) |> Seq.toArray)
        | JsonValueKind.String -> f <| node.GetValue<string>()
        | _ -> node.DeepClone()

    let skippedResult =
        { status = Skipped
          inputs = None
          outputs = None }

open SimulatorHelper

type LoopContextImpl(values: JsonNode list, disposeHook: LoopContext -> unit) as this =
    inherit LoopContext()

    [<DefaultValue>]
    val mutable values: JsonNode list

    do this.values <- values

    override this.Dispose() = disposeHook this

    override this.Advance() =
        this.values <- this.values.Tail
        this.values.IsEmpty |> not

    override this.Current = this.values.Head

type Simulator private (triggerOutput: JsonNode) =
    inherit SimulatorContext(triggerOutput)

    static member Trigger (logicApp: LogicAppSpec.Root) triggerOutput =
        let sim = new Simulator(triggerOutput)
        let result = sim.ExecuteGraph logicApp.definition.actions

        if sim.TerminationStatus.IsNone then
            sim.StopExecuting result

        sim

    member val TerminationStatus: Status option = None with get, set
    member val ActionResults = Dictionary<string, ActionResult>() with get, set
    member val LoopContextStack = Stack<LoopContextImpl>() with get, set
    member val ArrayOperationContextStack = Stack<LoopContextImpl>() with get, set

    override this.LoopContext = this.LoopContextStack.Peek()
    override this.ArrayOperationContext = this.ArrayOperationContextStack.Peek()

    override this.GetActionResult name =
        this.ActionResults.TryGetValue name
        |> function
            | true, result -> Some result
            | _ -> None

    member private this.RecordActionResult name result = this.ActionResults.[name] <- result

    override this.ExecuteGraph(actions: Map<string, 'a> when 'a :> IGraphExecutable) =
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
                        let result = action.Execute this
                        this.RecordActionResult actionName result

                        rest @ (getNextActions actionName)
                    | Completed ->
                        // This action's dependencies are in the wrong state, skip it
                        remainingActions.Remove actionName |> ignore
                        this.RecordActionResult actionName skippedResult
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
        let rec eval (expr: Expression) =
            let kv = expr |> Seq.exactlyOne

            match kv.Key with
            | "and" -> kv.Value |> arrayOfObjects |> Seq.forall eval
            | "or" -> kv.Value |> arrayOfObjects |> Seq.exists eval
            | "not" -> kv.Value.AsObject() |> eval |> not
            | LanguageCondition fn -> kv.Value.AsArray() |> List.ofSeq |> fn |> _.GetValue<bool>()
            | _ -> failwithf "Unexpected expression %A" expr

        eval expr

    override this.EvaluateLanguage expr =
        expr |> jsonMapStrs (LanguageEvaluator.evaluateIfNecessary this)

    override this.StopExecuting status = this.TerminationStatus <- Some status

    override this.ExternalServiceRequest request =
        printfn "External service request: %A" request
        ()

    override this.PushLoopContext(arg1: JsonNode seq) : LoopContext =
        this.PushLoopContext(this.LoopContextStack, arg1)

    override this.PushArrayOperationContext(arg1: JsonNode seq) : LoopContext =
        this.PushLoopContext(this.ArrayOperationContextStack, arg1)

    member this.PushLoopContext(stack: Stack<LoopContextImpl>, values: JsonNode seq) =
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
        |> List.iter (fun (name, _) -> this.RecordActionResult name skippedResult)
