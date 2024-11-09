module IllogicApps.Core.Program

open IllogicApps.Json
open ReadLogicApp
open IllogicApps.Simulator

let logicAppNames =
    [ "Stateful1"; "Stateful2"; "SkippingTest"; "FailurePropagationTest" ]

let logicApps =
    logicAppNames
    |> List.map (fun name -> name, readLogicApp $"{name}/workflow.json")

let runWorkflow =
    WorkflowFamily.buildWorkflowFamily
        (fun settings workflowHandler ->
            { settings with
                isBugForBugAccurate = true
                externalServiceHandlers =
                    [ workflowHandler
                      IllogicApps.JavaScript.Jint.jintJavascriptHandler
                      ExternalServices.loggingHandler
                      ExternalServices.noOpHandler ] })
        logicApps

let logicAppNamesStr = logicAppNames |> String.concat ", "
System.Console.WriteLine($"Enter workflow name (one of {logicAppNamesStr}):")
let userInput = System.Console.In.ReadLine()

let sims = runWorkflow userInput (Some(String "Test trigger"))

for sim in sims do
    printfn "\nWorkflow results for %s:\n------------------------\n" sim.WorkflowDetails.name

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
