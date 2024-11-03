module IllogicApps.Core.JsonUtil

open System.Collections.Generic
open System.Collections.Immutable
open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization
open IllogicApps.Json

type KVP = KeyValuePair<string, JsonNode>

type NumberSubtype =
    | Integer of int64
    | Float of float
    | Decimal of decimal

type Number2Subtype =
    | Integer2 of int64 * int64
    | Float2 of float * float
    | Decimal2 of decimal * decimal

let jsonNumberToSubtype (node: JsonNode) : NumberSubtype =
    match node with
    | null -> failwith "Expected number, got null"
    | n when n.GetValueKind() = JsonValueKind.Number ->
        let node = node.AsValue()

        match node.TryGetValue<int64>() with
        | true, i -> Integer i
        | _ ->
            match node.TryGetValue<float>() with
            | true, f -> Float f
            | _ ->
                match node.TryGetValue<decimal>() with
                | true, d -> Decimal d
                | _ -> failwithf "Expected number, got %A" (node.GetValue().GetType().Name)
    | kind -> failwithf "Expected number, got %A" kind

let promoteNums a b =
    match a, b with
    | Integer a, Integer b -> Integer2(a, b)
    | Integer a, Float b -> Float2(float a, b)
    | Integer a, Decimal b -> Decimal2(decimal a, b)
    | Float a, Integer b -> Float2(a, float b)
    | Float a, Float b -> Float2(a, b)
    | Float a, Decimal b -> Decimal2(decimal a, b)
    | Decimal a, Integer b -> Decimal2(a, decimal b)
    | Decimal a, Float b -> Decimal2(a, decimal b)
    | Decimal a, Decimal b -> Decimal2(a, b)

[<AutoOpen>]
type JsonOfHelper =
    static member inline jsonOf(i: int64) : JsonNode = JsonValue.Create i
    static member inline jsonOf(f: float) : JsonNode = JsonValue.Create f
    static member inline jsonOf(d: decimal) : JsonNode = JsonValue.Create d
    static member inline jsonOf(b: bool) : JsonNode = JsonValue.Create b
    static member inline jsonOf(s: string) : JsonNode = JsonValue.Create s
    static member inline jsonOf(a: JsonNode array) : JsonNode = JsonArray a
    static member inline jsonOf(a: JsonNode list) : JsonNode = JsonArray(List.toArray a)
    static member inline jsonOf(o: Map<string, JsonNode>) : JsonNode = JsonObject o
    static member inline jsonOf(o: (string * JsonNode) seq) : JsonNode = JsonObject(Map.ofSeq o)
    static member inline jsonNull: JsonNode = JsonValue.Create null

let safeClone (node: JsonNode) =
    match node with
    | null -> null
    | v -> v.DeepClone()

let rec illogicJsonOfSystemTextJson (node: JsonNode) : JsonTree =
    match node with
    | null -> Null
    | _ ->
        match node.GetValueKind() with
        | JsonValueKind.Object ->
            node.AsObject()
            |> Seq.map (fun kvp -> kvp.Key, illogicJsonOfSystemTextJson kvp.Value)
            |> Map.ofSeq
            |> Object
        | JsonValueKind.Array ->
            node.AsArray()
            |> Seq.map illogicJsonOfSystemTextJson
            |> ImmutableArray.CreateRange
            |> Array
        | JsonValueKind.Number ->
            let node = node.AsValue()

            match node.TryGetValue<int64>() with
            | true, i -> JsonTree.Integer i
            | _ ->
                match node.TryGetValue<float>() with
                | true, f -> JsonTree.Float f
                | _ ->
                    match node.TryGetValue<decimal>() with
                    | true, d -> JsonTree.Decimal d
                    | _ -> failwithf "Expected number, got %A" (node.GetValue().GetType().Name)
        | JsonValueKind.String -> String(node.GetValue<string>())
        | JsonValueKind.True -> Boolean true
        | JsonValueKind.False -> Boolean false
        | JsonValueKind.Null -> Null
        | _ -> failwithf "Unsupported value kind %A" (node.GetValueKind())

let rec systemTextJsonOfIllogicJson (tree: JsonTree) : JsonNode =
    match tree with
    | Null -> null
    | Object o ->
        o
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> KVP(k, systemTextJsonOfIllogicJson v))
        |> JsonObject
        :> JsonNode
    | Array a -> JsonArray(a |> Seq.map systemTextJsonOfIllogicJson |> Seq.toArray)
    | String s -> JsonValue.Create s
    | JsonTree.Integer i -> JsonValue.Create i
    | JsonTree.Float f -> JsonValue.Create f
    | JsonTree.Decimal d -> JsonValue.Create d
    | Boolean b -> JsonValue.Create b

let rec systemTextJsonOfIllogicJsonIgnoringNulls (tree: JsonTree) : JsonNode =
    match tree with
    | Null -> null
    | Object o ->
        o
        |> Map.toSeq
        |> Seq.filter (fun (_, v) -> v <> Null)
        |> Seq.map (fun (k, v) -> KVP(k, systemTextJsonOfIllogicJson v))
        |> JsonObject
        :> JsonNode
    | Array a -> JsonArray(a |> Seq.map systemTextJsonOfIllogicJson |> Seq.toArray)
    | String s -> JsonValue.Create s
    | JsonTree.Integer i -> JsonValue.Create i
    | JsonTree.Float f -> JsonValue.Create f
    | JsonTree.Decimal d -> JsonValue.Create d
    | Boolean b -> JsonValue.Create b

type IllogicToSystemTextSerializerAdapter() =
    inherit JsonConverter<JsonTree>()

    override this.Read(reader, typeToConvert, options) =
        JsonSerializer.Deserialize<JsonNode>(&reader, options)
        |> illogicJsonOfSystemTextJson

    override this.Write(writer, value, options) =
        let value = systemTextJsonOfIllogicJsonIgnoringNulls value
        JsonSerializer.Serialize(writer, value, options)

let sensibleSerialiserOptions =
    let options =
        JsonSerializerOptions(
            defaults = JsonSerializerDefaults.General,
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping,
            WriteIndented = false,
            NumberHandling = JsonNumberHandling.AllowNamedFloatingPointLiterals
        )

    options.Converters.Add(IllogicToSystemTextSerializerAdapter())
    options
