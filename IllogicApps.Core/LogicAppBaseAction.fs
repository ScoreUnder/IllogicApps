module IllogicApps.Core.LogicAppBaseAction

open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization

[<AbstractClass>]
type BaseAction() =
    interface IGraphExecutable with
        member this.Execute(context: SimulatorContext) = this.Execute(context)
        member this.RunAfter = this.RunAfter

    [<JsonPropertyName("type")>]
    member val ActionType = "" with get, set

    member val RunAfter: Map<string, Status list> option = None with get, set

    abstract member Execute: SimulatorContext -> ActionResult
    abstract member GetChildren: unit -> BaseAction list
    default this.GetChildren() = []

type UnknownAction() =
    inherit BaseAction()
    member val Original = JsonObject() with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Unknown action: %A" this.Original

        { status = Skipped
          inputs = None
          outputs = None }

type ActionResolver(actionsNamespace: string) =
    inherit JsonConverter<BaseAction>()

    override this.Read(reader: Utf8JsonReader byref, typeToConvert: System.Type, options: JsonSerializerOptions) =
        let jsonObj = JsonSerializer.Deserialize<JsonObject>(&reader, options) in

        if jsonObj = null then
            raise <| new JsonException("Action is null")

        let actionType =
            match jsonObj.TryGetPropertyValue "type" with
            | true, v -> v
            | _ -> raise <| new JsonException("Missing 'type' property in action object")

        let actionTypeStr = actionType.ToString() in

        try
            $"{actionsNamespace}.{actionTypeStr}"
            |> this.GetType().Assembly.GetType
            |> Option.ofObj
            |> function
                | Some ty -> JsonSerializer.Deserialize(jsonObj, ty, options) :?> BaseAction
                | None ->
                    let act = JsonSerializer.Deserialize<UnknownAction>(jsonObj, options)
                    act.Original <- jsonObj
                    act :> BaseAction
        with ex ->
            raise (
                new JsonException($"Failed to deserialize action of type '{actionTypeStr}', as object {jsonObj}", ex)
            )

    override this.Write(writer: Utf8JsonWriter, value: BaseAction, options: JsonSerializerOptions) =
        raise (new System.NotImplementedException())
