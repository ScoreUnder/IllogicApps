namespace IllogicApps.Simulator

open System.Collections.Generic
open System.Text.Json.Nodes
open IllogicApps.Core
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
        | :? JsonArray as a -> a |> Seq.map (fun o -> o.AsObject())
        | _ -> failwithf "Expected array of objects, got %A" node

open SimulatorHelper

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
                            this.ActionResults.[actionName] <- result
                            let nextActions = rest @ (getNextActions actionName)

                            executeNext (mergeStatus overallResult result.status) nextActions
                        else
                            executeNext overallResult rest

        executeNext Succeeded (getNextActions "")


    override this.EvaluateCondition expr =
        let rec eval (expr: Expression) =
            let kv = expr |> Seq.exactlyOne

            match kv.Key with
            | "and" -> kv.Value |> arrayOfObjects |> Seq.forall eval
            | "or" -> kv.Value |> arrayOfObjects |> Seq.exists eval
            | "not" -> kv.Value.AsObject() |> eval |> not
            | LanguageCondition fn -> kv.Value.AsArray() |> List.ofSeq |> fn |> (fun f -> f.GetValue<bool>())
            | _ -> failwithf "Unexpected expression %A" expr

        eval expr

    override this.EvaluateLanguage expr =
        printfn "NOT IMPLEMENTED EvaluateLanguage: %A" expr
        // TODO
        expr

    override this.StopExecuting status = this.TerminationStatus <- Some status
