module IllogicApps.Core.Program

open IllogicApps.Json
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

let sim = Simulator.TriggerSimple logicApp (Some (String "FAKE INPUT"))

printfn "\nAction results:\n---------------\n"
sim.ActionResults
|> Seq.map (fun (KeyValue(k, v)) -> k, CompletedStepTypes.jsonOfCompletedAction v)
|> Conversions.createObject
|> Conversions.writePrettyJson System.Console.Out

printfn "\nVariable results:\n-----------------\n"
sim.Variables
|> Seq.map (fun (KeyValue(k, v)) -> k, v)
|> Conversions.createObject
|> Conversions.writePrettyJson System.Console.Out
