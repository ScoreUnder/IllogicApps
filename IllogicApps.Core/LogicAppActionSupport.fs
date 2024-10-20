module IllogicApps.Core.LogicAppActionSupport

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization
open IllogicApps.Core.LogicAppSpec

type SetVariableSingle() =
    member val Name: string = "" with get, set
    member val Value: JsonNode = JsonValue.Create(null) with get, set

type VariableType =
    | String
    | Integer
    | Float
    | Boolean
    | Object
    | Array

type InitializeVariableSingle() =
    member val Name: string = "" with get, set
    member val Value: JsonNode option = None with get, set

    [<JsonPropertyName("type")>]
    member val VariableType: VariableType = Object with get, set

type 'a VariablesInputs = { variables: 'a list }

type HttpResponseInputs =
    { body: JsonNode option
      headers: IDictionary<string, string> option
      statusCode: int }

type SwitchDefault() =
    member val Actions: ActionGraph = Map.empty with get, set

type SwitchCase() =
    inherit SwitchDefault()

    member val Case: JsonNode = JsonValue.Create(null) with get, set

type UntilLimit = { count: int; timeout: string }

type TerminateRunError =
    { code: string option
      message: string option }

type TerminateInputs =
    { runStatus: string
      runError: TerminateRunError option }

let defaultExpression () : Expression = new JsonObject()

let makeObject (pairs: (string * JsonNode) seq) : JsonNode =
    new JsonObject(pairs |> Seq.map (fun (k, v) -> new KeyValuePair<string, JsonNode>(k, v)))

let defaultForType typ : JsonNode =
    match typ with
    | String -> JsonValue.Create("")
    | Integer -> JsonValue.Create(0) // TODO verify
    | Float -> JsonValue.Create(0.0) // TODO verify
    | Boolean -> JsonValue.Create(false) // TODO verify
    | Object -> JsonValue.Create(null) // TODO verify
    | Array -> new JsonArray() // TODO verify

let coerce (typ: VariableType) (value: JsonNode) : JsonNode =
    try
        match typ with
        | String -> JsonValue.Create(value.ToString())
        | Integer -> JsonValue.Create(value.GetValue<int>())
        | Float -> JsonValue.Create(value.GetValue<double>())
        | Boolean -> JsonValue.Create(value.GetValue<bool>())
        | Object ->
            begin
                match value.GetValueKind() with
                | JsonValueKind.Object -> value
                | JsonValueKind.Null -> value
                | _ -> raise <| new InvalidOperationException()
            end
        | Array ->
            if value.GetValueKind() = JsonValueKind.Array then
                value
            else
                raise <| new InvalidOperationException()
    with
    | :? NullReferenceException
    | :? InvalidOperationException
    | :? FormatException ->
        raise
        <| new InvalidOperationException($"Failed to coerce value {value} to type {typ}")

let toKvp (k: 'a, v: 'b) = new KeyValuePair<'a, 'b>(k, v)
let fromKvp (kvp: KeyValuePair<'a, 'b>) = kvp.Key, kvp.Value
let toKvps seq = Seq.map toKvp seq
let fromKvps seq = Seq.map fromKvp seq
