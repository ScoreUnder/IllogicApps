module IllogicApps.Core.LogicAppActionSupport

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization
open IllogicApps.Core.LogicAppSpec

let toKvp (k: 'a, v: 'b) = new KeyValuePair<'a, 'b>(k, v)
let fromKvp (kvp: KeyValuePair<'a, 'b>) = kvp.Key, kvp.Value
let toKvps seq = Seq.map toKvp seq
let fromKvps seq = Seq.map fromKvp seq

let makeObject (pairs: (string * JsonNode) seq) : JsonNode =
    new JsonObject(pairs |> Seq.map (fun (k, v) -> new KeyValuePair<string, JsonNode>(k, v)))

let optionalAddKey (key: string) (value: JsonNode option) (pairs: (string * JsonNode) list) =
    match value with
    | Some v -> (key, v.DeepClone()) :: pairs
    | None -> pairs

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
    [<JsonPropertyName("name")>]
    member val Name: string = "" with get, set

    [<JsonPropertyName("value")>]
    member val Value: JsonNode option = None with get, set

    [<JsonPropertyName("type")>]
    member val VariableType: VariableType = Object with get, set

type 'a VariablesInputs = { variables: 'a list }

type ParseJsonInputs = { content: JsonNode; schema: JsonNode }

type QueryInputs = { from: JsonNode; where: JsonNode }

type HttpResponseInputs =
    { body: JsonNode option
      headers: IDictionary<string, string> option
      statusCode: JsonNode }

type ActionGraphContainer() =
    member val Actions: ActionGraph = Map.empty with get, set

type SwitchCase() =
    inherit ActionGraphContainer()

    member val Case: JsonNode = JsonValue.Create(null) with get, set

type UntilLimit = { count: int; timeout: string }

type TerminateRunError =
    { code: string option
      message: string option }

type TerminateInputs =
    { runStatus: string
      runError: TerminateRunError option }

type HttpInputs =
    { method: string
      uri: string
      headers: Map<string, string> option
      queries: Map<string, string> option
      body: JsonNode option
      cookie: string option
      authentication: JsonObject option }

let defaultExpression () : Expression = new JsonObject()

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
        | Float -> JsonValue.Create(value.GetValue<float>())
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

let getVarTypechecked (context: SimulatorContext) var typ =
    if not (context.Variables.ContainsKey(var)) then
        failwithf "Variable '%s' does not exist" var

    let originalValue = context.Variables.[var]

    if originalValue.GetValueKind() <> typ then
        failwithf "Variable is of type %A, expected %A" (originalValue.GetValueKind()) typ

    originalValue
