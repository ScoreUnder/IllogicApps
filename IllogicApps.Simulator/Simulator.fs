namespace IllogicApps.Simulator

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Nodes
open IllogicApps.Core
open IllogicApps.Core.LogicAppBaseAction
open IllogicApps.Simulator.LanguageCondition

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

    let areDependenciesSatisfied (actionResults: IDictionary<string, ActionResult>) (action: IGraphExecutable) =
        action.RunAfter
        |> Option.defaultValue emptyDependencyList
        |> Seq.forall (fun kv ->
            let requiredStatuses = kv.Value

            match kv.Key with
            | "" -> true
            | depName ->
                match actionResults.TryGetValue(depName) with
                | true, actionResult -> Seq.contains actionResult.status requiredStatuses
                | _ -> false)

    let areDependenciesCompleted (actionResults: IDictionary<string, ActionResult>) (action: IGraphExecutable) =
        action.RunAfter
        |> Option.defaultValue emptyDependencyList
        |> Seq.forall (fun kv ->
            match kv.Key with
            | "" -> true
            | depName -> actionResults.ContainsKey(depName))

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

    let evaluateLanguageStr simContext (str: string) : JsonNode =
        if str.StartsWith("@") && not (str.StartsWith("@{")) then
            // Whole thing needs replacing with the output of the expression
            printfn "Not implemented: expression evaluation: %A" str
            JsonValue.Create(str)
        else if str.Contains("@") then
            // Always a string, but may contain sub-expressions
            printfn "Not implemented: sub-expression evaluation: %A" str
            JsonValue.Create(str)
        else
            JsonValue.Create(str)

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

    member private this.RecordActionResult name result = this.ActionResults.[name] <- result

    override this.ExecuteGraph(actions: Map<string, 'a> when 'a :> IGraphExecutable) =
        let dependencyGraph = createDependencyGraph actions
        let remainingActions = new Dictionary<string, 'a>(actions)

        let getNextActions name =
            Map.tryFind name dependencyGraph |> Option.defaultValue []

        let rec executeNext overallResult actionQueue =
            if this.TerminationStatus.IsSome then
                overallResult
            else
                match actionQueue with
                | [] -> overallResult
                | actionName :: rest ->
                    match remainingActions.TryGetValue actionName with
                    | false, _ -> executeNext overallResult rest
                    | true, action ->
                        if areDependenciesSatisfied this.ActionResults action then
                            remainingActions.Remove actionName |> ignore
                            let result = action.Execute this
                            this.RecordActionResult actionName result

                            let nextActions = rest @ (getNextActions actionName)
                            executeNext (mergeStatus overallResult result.status) nextActions
                        else if areDependenciesCompleted this.ActionResults action then
                            // This action's dependencies are in the wrong state, skip it
                            remainingActions.Remove actionName |> ignore
                            this.RecordActionResult actionName skippedResult

                            let nextActions = rest @ (getNextActions actionName)
                            executeNext overallResult nextActions
                        else
                            // This action's dependencies are not yet complete, try again once something else finishes
                            // (it's ok to not put it back in the queue, as it will be re-added when its next dependency is completed)
                            executeNext overallResult rest

        executeNext Succeeded (getNextActions "")


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
        expr |> jsonMapStrs (evaluateLanguageStr this)

    override this.StopExecuting status = this.TerminationStatus <- Some status

    override this.ExternalServiceRequest request =
        printfn "External service request: %A" request
        ()

    override this.PushLoopContext(arg1: JsonNode seq) : LoopContext =
        let loopContext =
            new LoopContextImpl(List.ofSeq arg1, this.PopAndCompareLoopContext)

        this.LoopContextStack.Push(loopContext)
        loopContext

    member this.PopAndCompareLoopContext context =
        let top = this.LoopContextStack.Peek(): LoopContext

        if top = context then
            this.LoopContextStack.Pop() |> ignore
        else
            raise <| new InvalidOperationException("Loop context push/pop mismatch")

    override this.ForceSkipAll actions =
        actions
        |> Seq.map (fun kvp -> kvp.Key, (kvp.Value: IGraphExecutable))
        |> Seq.toList
        |> getAllChildren
        |> List.iter (fun (name, _) -> this.RecordActionResult name skippedResult)
