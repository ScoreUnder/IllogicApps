module IllogicApps.Core.ReadLogicApp

open IllogicApps.Core.CompletedStepTypes
open IllogicApps.Core.LogicAppActions
open IllogicApps.Json

type UnknownAction(json) =
    inherit BaseAction(json)
    member val Original = json with get

    override this.Execute (_: string) (_: SimulatorContext) =
        printfn "Unknown action: %s" <| Conversions.prettyStringOfJson this.Original

        { ActionResult.Default with
            status = Failed
            code = Some ActionFailed
            error =
                Some
                    { code = ActionFailed
                      message = sprintf "Unknown action type %s" this.ActionType } }

let actionMap =
    Map.ofList<string, (JsonTree -> BaseAction) -> JsonTree -> BaseAction>
        [ "Request", (fun _ v -> Request v)
          "Scope", (fun a v -> Scope(a, v))
          "If", (fun a v -> If(a, v))
          "Foreach", (fun a v -> ForEach(a, v))
          "Switch", (fun a v -> Switch(a, v))
          "Until", (fun a v -> Until(a, v))
          "Terminate", (fun _ v -> Terminate v)
          "InitializeVariable", (fun _ v -> InitializeVariable v)
          "SetVariable", (fun _ v -> SetVariable v)
          "AppendToStringVariable", (fun _ v -> AppendToStringVariable v)
          "AppendToArrayVariable", (fun _ v -> AppendToArrayVariable v)
          "IncrementVariable", (fun _ v -> IncrementVariable v)
          "DecrementVariable", (fun _ v -> DecrementVariable v)
          "Compose", (fun _ v -> Compose v)
          "ParseJson", (fun _ v -> ParseJson v)
          "Query", (fun _ v -> Query v)
          "Select", (fun _ v -> Select v)
          "JavaScriptCode", (fun _ v -> JavaScriptCode v)
          "ServiceProvider", (fun _ v -> ServiceProvider v)
          "Response", (fun _ v -> Response v)
          "Http", (fun _ v -> Http v)
          "Workflow", (fun _ v -> Workflow v) ]

let rec resolveAction (json: JsonTree) =
    let type_ = JsonTree.getKey "type" json |> Conversions.ensureString

    let parseAction =
        actionMap
        |> Map.tryFind type_
        |> Option.defaultValue (fun _ v -> UnknownAction v)

    parseAction resolveAction json

let decodeLogicApp (json: string) =
    Parser.parse json |> LogicAppSpec.rootOfJson resolveAction

let readLogicApp path =
    System.IO.File.ReadAllText path |> decodeLogicApp
