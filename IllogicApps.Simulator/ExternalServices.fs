module IllogicApps.Simulator.ExternalServices

open IllogicApps.Core
open IllogicApps.Core.ExternalServiceTypes

type ExternalServiceHandler = SimulatorContext -> ExternalServiceRequest -> bool

let runAllHandlers (handlers: ExternalServiceHandler list) (sim: SimulatorContext) (request: ExternalServiceRequest) =
    handlers |> List.exists (fun h -> h sim request)

let loggingHandler (_sim: SimulatorContext) (request: ExternalServiceRequest) =
    printfn "Request: %A" request
    false

let noOpHandler (_sim: SimulatorContext) (_request: ExternalServiceRequest) = true
