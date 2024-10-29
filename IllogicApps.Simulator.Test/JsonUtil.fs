module IllogicApps.Simulator.Test.JsonUtil

open System
open System.Collections
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Nodes

open IllogicApps.Simulator

type KVP = KeyValuePair<string, JsonNode>

let convertUpPrimitive (v: obj) =
    match v with
    | :? int32 as i -> box <| int64 i
    | :? int16 as i -> box <| int64 i
    | :? int8 as i -> box <| int64 i
    | :? uint32 as i -> box <| int64 i
    | :? uint16 as i -> box <| int64 i
    | :? uint8 as i -> box <| int64 i
    | :? uint64 as i -> box <| int64 i
    | :? single as f -> box <| float f
    | :? Half as f -> box <| float f
    | v -> v

/// Convert a JsonNode (hiding JsonElement values) to a real JsonNode
let reify (jsonNode: JsonNode) =
    let rec reify' (element: JsonElement) : JsonNode =
        match element.ValueKind with
        | JsonValueKind.Object ->
            element.EnumerateObject()
            |> Seq.map (fun prop -> new KVP(prop.Name, reify' prop.Value))
            |> Seq.toList
            |> fun p -> JsonObject p
        | JsonValueKind.Array ->
            element.EnumerateArray()
            |> Seq.map reify'
            |> Seq.toArray
            |> fun a -> JsonArray a
        | JsonValueKind.True -> JsonValue.Create true
        | JsonValueKind.False -> JsonValue.Create false
        | JsonValueKind.Null -> JsonValue.Create null
        | JsonValueKind.Number ->
            match element.TryGetInt64() with
            | true, i -> JsonValue.Create i
            | _ ->
                match element.TryGetDouble() with
                | true, f -> JsonValue.Create f
                | _ -> JsonValue.Create(element.GetDecimal())
        | JsonValueKind.String -> JsonValue.Create(element.GetString())
        | _ -> failwithf "Unsupported value kind %A" element.ValueKind

    match jsonNode.GetValue<obj>() with
    | :? JsonElement as element -> reify' element
    | _ -> jsonNode

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

let rec jsonOfObject (object: obj) =
    match convertUpPrimitive object with
    | :? IDictionary as map ->
        JsonObject(
            map
            |> Seq.cast<DictionaryEntry>
            |> Seq.map (fun kvp -> kvp.Key :?> string, jsonOfObject kvp.Value)
            |> Map.ofSeq
        )
    | :? string as str -> JsonValue.Create str
    | :? IEnumerable as seq -> JsonArray(seq |> Seq.cast<obj> |> Seq.map jsonOfObject |> Seq.toArray)
    | :? int64 as i -> JsonValue.Create i
    | :? float as f -> JsonValue.Create f
    | :? decimal as d -> JsonValue.Create d
    | :? bool as b -> JsonValue.Create b
    | null -> JsonValue.Create null
    | _ -> failwithf "Unsupported object type %A" (object.GetType())

let inline jsonOf (value: obj) : JsonNode =
    match value with
    | :? int64 as i -> JsonValue.Create i
    | :? float as f -> JsonValue.Create f
    | :? decimal as d -> JsonValue.Create d
    | :? bool as b -> JsonValue.Create b
    | :? string as s -> JsonValue.Create s
    | :? (JsonNode array) as a -> JsonArray a
    | :? Map<string, JsonNode> as o -> JsonObject o
    | null -> JsonValue.Create null
    | _ -> failwith "Unsupported type"

let inline (!@) o = jsonOf o

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
        // Todo: we rely on simulator modules axiomatically while testing simulator code
        match
            BuiltinFunctions.promoteNums
                (BuiltinFunctions.jsonNumberToSubtype <| reify a)
                (BuiltinFunctions.jsonNumberToSubtype <| reify b)
        with
        | BuiltinFunctions.Integer2(a, b) -> a = b
        | BuiltinFunctions.Float2(a, b) -> a = b
        | BuiltinFunctions.Decimal2(a, b) -> a = b
    | _ -> a.GetValue() = b.GetValue()
