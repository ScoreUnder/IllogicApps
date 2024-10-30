module IllogicApps.Core.JsonUtil

open System.Collections.Generic
open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Nodes

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
    match node.GetValueKind() with
    | JsonValueKind.Number ->
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

let rec jsonToObject (jsonNode: JsonNode) =
    match jsonNode.GetValueKind() with
    | JsonValueKind.Object ->
        jsonNode.AsObject()
        |> Seq.map (fun kvp -> kvp.Key, jsonToObject kvp.Value)
        |> Map.ofSeq
        :> obj
    | JsonValueKind.Array -> jsonNode.AsArray() |> Seq.map jsonToObject |> Array.ofSeq :> obj
    | JsonValueKind.True
    | JsonValueKind.False
    | JsonValueKind.Null
    | JsonValueKind.Number
    | JsonValueKind.String -> jsonNode.GetValue()
    | JsonValueKind.Undefined -> failwithf "Undefined value in JSON at %s" (jsonNode.GetPath())
    | _ -> failwithf "Unsupported value kind %A" (jsonNode.GetValueKind())

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

let rec jsonsEqual (a: JsonNode) (b: JsonNode) =
    match a.GetValueKind(), b.GetValueKind() with
    | JsonValueKind.Object, JsonValueKind.Object ->
        let aObj = a.AsObject()
        let bObj = b.AsObject()
        let aSorted = aObj |> Seq.sortBy _.Key |> Seq.toList
        let bSorted = bObj |> Seq.sortBy _.Key |> Seq.toList
        let aKeys = aSorted |> Seq.map _.Key |> Set.ofSeq
        let bKeys = bSorted |> Seq.map _.Key |> Set.ofSeq

        aKeys = bKeys
        && Seq.forall2 (fun (a: KVP) (b: KVP) -> jsonsEqual a.Value b.Value) aSorted bSorted
    | JsonValueKind.Array, JsonValueKind.Array ->
        let aArr = a.AsArray()
        let bArr = b.AsArray()
        aArr.Count = bArr.Count && Seq.forall2 jsonsEqual aArr bArr
    | JsonValueKind.Number, JsonValueKind.Number ->
        match promoteNums (jsonNumberToSubtype a) (jsonNumberToSubtype b) with
        | Integer2(a, b) -> a = b
        | Float2(a, b) -> a = b
        | Decimal2(a, b) -> a = b
    | JsonValueKind.True, JsonValueKind.True
    | JsonValueKind.False, JsonValueKind.False
    | JsonValueKind.Null, JsonValueKind.Null -> true
    | ta, tb when ta = tb -> a.GetValue().Equals(b.GetValue())
    | _ -> false

let sensibleSerialiserOptions =
    JsonSerializerOptions(
        defaults = JsonSerializerDefaults.General,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    )
