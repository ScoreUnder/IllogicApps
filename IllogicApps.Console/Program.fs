module IllogicApps.Core.Program

open IllogicApps.Json
open ReadLogicApp
open IllogicApps.Simulator

let examplePath = "Stateful1/workflow.json"

// let logicApp = readLogicApp examplePath
// let logicApp = readLogicApp <| examplePath.Replace("1", "2")
let logicApp = readLogicApp <| examplePath.Replace("Stateful1", "SkippingTest")
// let logicApp = readLogicApp <| examplePath.Replace("Stateful1", "FailurePropagationTest")

let sim =
    Simulator.Trigger
        logicApp.definition.actions
        { SimulatorCreationOptions.createSimple logicApp (Some(String "FAKE INPUT")) with
            externalServiceHandlers =
                [ IllogicApps.JavaScript.Jint.jintJavascriptHandler
                  ExternalServices.loggingHandler ] }

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
