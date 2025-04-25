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

type UnknownTrigger(json) =
    inherit BaseTrigger(json)

    member val Inputs = JsonTree.tryGetKey "inputs" json with get

    override this.ProcessInputs context =
        this.Inputs |> Option.map context.EvaluateLanguage

    override this.RunFromRequest request context =
        // If this isn't implemented it's probably a timer trigger or something
        printfn "WARN: Unimplemented Trigger triggered with %O" request

        // However we still want to be able to give outputs just in case they're necessary
        { ActionResult.Default with
            status = Succeeded
            inputs = this.ProcessInputs context
            outputs = Some(request.ToJson()) }

let triggerMap =
    Map.ofList<string, JsonTree -> BaseTrigger> [ "Request", (fun v -> Request v) ]

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
          "InvokeFunction", (fun _ v -> InvokeFunction v)
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

let resolveTrigger (json: JsonTree) =
    let type_ = JsonTree.getKey "type" json |> Conversions.ensureString

    let parseAction =
        triggerMap
        |> Map.tryFind type_
        |> Option.defaultValue (fun v -> UnknownTrigger v)

    parseAction json

let decodeLogicApp (json: string) =
    JsonParser.parse json |> LogicAppSpec.rootOfJson resolveTrigger resolveAction

let readLogicApp path =
    System.IO.File.ReadAllText path |> decodeLogicApp
