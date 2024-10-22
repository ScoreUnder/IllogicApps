module IllogicApps.Core.Program

open System.Text.Json.Nodes
open ReadLogicApp
open IllogicApps.Simulator

let examplePath = "Stateful1/workflow.json"

// let logicApp = readLogicApp examplePath
// let logicApp = readLogicApp <| examplePath.Replace("1", "2")
let logicApp = readLogicApp <| examplePath.Replace("Stateful1", "SkippingTest")
// let logicApp = readLogicApp <| examplePath.Replace("Stateful1", "FailurePropagationTest")

// printfn "%A" logicApp

// logicApp.definition.actions
// |> LogicAppActionSupport.fromKvps
// |> Seq.toList
// |> LogicAppBaseAction.getAllChildren
// |> printfn "%A"

let sim = Simulator.Trigger logicApp (JsonValue.Create("FAKE INPUT"))

let opts = makeJsonSerializerOptions()
opts.WriteIndented <- true

printfn "\nAction results:\n---------------\n"
sim.ActionResults
|> fun c -> System.Text.Json.JsonSerializer.Serialize(c, opts)
|> printfn "%s"

printfn "\nVariable results:\n-----------------\n"
sim.Variables
|> fun c -> System.Text.Json.JsonSerializer.Serialize(c, opts)
|> printfn "%s"

if sim.LoopContextStack.Count <> 0 then failwith "Loop context stack not empty"
if sim.ArrayOperationContextStack.Count <> 0 then failwith "Scope context stack not empty"
