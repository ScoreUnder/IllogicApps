module IllogicApps.Core.Program

open System.IO

open IllogicApps.Core.ExternalServiceTypes
open IllogicApps.Json
open ReadLogicApp
open IllogicApps.Simulator

let workflowFiles =
    "TestWorkflows"
    |> Directory.EnumerateDirectories
    |> Seq.map (fun dir -> Path.Combine(dir, "workflow.json"))
    |> Seq.filter File.Exists
    |> Array.ofSeq

let getLogicAppName (path: string) =
    path |> Path.GetDirectoryName |> Path.GetFileName

let logicApps =
    workflowFiles |> Seq.map (fun path -> getLogicAppName path, readLogicApp path)

let runWorkflow =
    WorkflowFamily.buildWorkflowFamily
        (fun settings workflowHandler ->
            { settings with
                isBugForBugAccurate = true
                externalServiceHandlers =
                    [ workflowHandler
                      IllogicApps.JavaScript.Jint.Handler.jintJavascriptHandler
                      ExternalServices.loggingHandler
                      ExternalServices.noOpHandler ] })
        logicApps

let logicAppNamesStr =
    workflowFiles |> Seq.map getLogicAppName |> String.concat ", "

System.Console.WriteLine($"Enter workflow name (one of {logicAppNamesStr}):")
let userInput = System.Console.In.ReadLine()

let sims = runWorkflow userInput (TriggerCompletion.Invoked HttpRequest.Default)

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
