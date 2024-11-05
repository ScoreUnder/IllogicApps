module IllogicApps.Core.ReadLogicApp

open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization

open IllogicApps.Core.LogicAppBaseAction

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
                    act
                    : BaseAction
        with ex ->
            raise (
                new JsonException($"Failed to deserialize action of type '{actionTypeStr}', as object {jsonObj}", ex)
            )

    override this.Write(writer: Utf8JsonWriter, value: BaseAction, options: JsonSerializerOptions) =
        raise (new System.NotImplementedException())

let actionResolver = ActionResolver("IllogicApps.Core.LogicAppActions")

let makeJsonSerializerOptions () =
    let options = JsonUtil.sensibleSerialiserOptions () in
    options.Converters.Add(actionResolver)
    options

let readLogicApp path =
    use f = System.IO.File.OpenRead path
    JsonSerializer.Deserialize<LogicAppSpec.Root>(f, makeJsonSerializerOptions ())

let decodeLogicApp (json: string) =
    JsonSerializer.Deserialize<LogicAppSpec.Root>(json, makeJsonSerializerOptions ())
