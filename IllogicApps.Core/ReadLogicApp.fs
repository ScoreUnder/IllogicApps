module IllogicApps.Core.ReadLogicApp

open IllogicApps.Core.LogicAppActions
open IllogicApps.Core.LogicAppBaseAction
open IllogicApps.Json

let actionMap =
    Map.ofList<string, (JsonTree -> BaseAction) -> JsonTree -> BaseAction>
        [ "Request", (fun _ v -> Request.OfJson v)
          "Scope", (fun a v -> Scope.OfJson a v)
          "If", (fun a v -> If.OfJson a v)
          "Switch", (fun a v -> Switch.OfJson a v)
          "Until", (fun a v -> Until.OfJson a v)
          "Terminate", (fun _ v -> Terminate.OfJson v)
          "InitializeVariable", (fun _ v -> InitializeVariable.OfJson v)
          "SetVariable", (fun _ v -> SetVariable.OfJson v)
          "AppendToStringVariable", (fun _ v -> AppendToStringVariable.OfJson v)
          "AppendToArrayVariable", (fun _ v -> AppendToArrayVariable.OfJson v)
          "IncrementVariable", (fun _ v -> IncrementVariable.OfJson v)
          "DecrementVariable", (fun _ v -> DecrementVariable.OfJson v)
          "Compose", (fun _ v -> Compose.OfJson v)
          "ParseJson", (fun _ v -> ParseJson.OfJson v)
          "Query", (fun _ v -> Query.OfJson v)
          "JavaScriptCode", (fun _ v -> JavaScriptCode.OfJson v)
          "Response", (fun _ v -> Response.OfJson v)
          "Http", (fun _ v -> Http.OfJson v)
          "Workflow", (fun _ v -> Workflow.OfJson v) ]

let rec resolveAction (json: JsonTree) =
    let type_ = JsonTree.getKey "type" json |> Conversions.ensureString

    let parseAction =
        actionMap
        |> Map.tryFind type_
        |> Option.defaultValue (fun _ v -> UnknownAction.OfJson v)

    parseAction resolveAction json

let decodeLogicApp (json: string) =
    Parser.parse json |> LogicAppSpec.rootOfJson resolveAction

let readLogicApp path =
    System.IO.File.ReadAllText path |> decodeLogicApp
