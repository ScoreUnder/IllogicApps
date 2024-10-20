module IllogicApps.Core.Program

open System.Text.Json.Nodes
open ReadLogicApp
open IllogicApps.Simulator

let examplePath = "/home/score/src/IllogicApps/Stateful1/workflow.json"

let logicApp = readLogicApp examplePath

printfn "%A" logicApp

Simulator.Trigger logicApp (JsonValue.Create("FAKE INPUT")) |> ignore
