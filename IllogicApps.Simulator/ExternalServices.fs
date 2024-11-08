module IllogicApps.Simulator.ExternalServices

open IllogicApps.Core.ExternalServiceTypes

type ExternalServiceHandler = ExternalServiceRequest -> bool

let runAllHandlers (handlers: ExternalServiceHandler list) (request: ExternalServiceRequest) =
    handlers |> List.exists (fun h -> h request)

let loggingHandler (request: ExternalServiceRequest) =
    printfn "Request: %A" request
    false

let noOpHandler (_request: ExternalServiceRequest) = true
