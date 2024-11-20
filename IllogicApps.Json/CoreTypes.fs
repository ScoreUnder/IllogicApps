[<AutoOpen>]
module IllogicApps.Json.CoreTypes

open System.Collections.Immutable

open IllogicApps.Json.SerialisationHelper

type JsonTree =
    | Null
    | Object of OrderedMap<string, JsonTree>
    | Array of JsonTree ImmutableArray
    | String of string
    | Integer of int64
    | Float of float
    | Decimal of decimal
    | Boolean of bool

    override json.ToString() =
        let rec aux acc json =
            match json with
            | Null -> "null" :: acc
            | Array a when a.IsEmpty -> "[]" :: acc
            | Array a -> "[" :: List.tail (Seq.foldBack (fun el acc -> "," :: aux acc el) a ("]" :: acc))
            | Object o when OrderedMap.isEmpty o -> "{}" :: acc
            | Object o ->
                "{"
                :: List.tail (
                    OrderedMap.foldBack
                        (fun k v acc -> "," :: "\"" :: (escapeStringForJson k) :: "\":" :: aux acc v)
                        o
                        ("}" :: acc)
                )
            | String s -> "\"" :: (escapeStringForJson s) :: "\"" :: acc
            | Integer i -> string i :: acc
            | Float f when System.Double.IsNaN f -> "\"NaN\"" :: acc
            | Float f when System.Double.IsNegativeInfinity f -> "\"-Infinity\"" :: acc
            | Float f when System.Double.IsPositiveInfinity f -> "\"Infinity\"" :: acc
            | Float f -> hackyInsertDecimalPoint (string f) :: acc
            | Decimal d -> hackyInsertDecimalPoint (string d) :: acc
            | Boolean true -> "true" :: acc
            | Boolean false -> "false" :: acc

        aux [] json |> String.concat ""

[<RequireQualifiedAccess>]
type JsonType =
    | Null
    | Object
    | Array
    | String
    | Integer
    | Float
    | Decimal
    | Boolean

module JsonTree =
    let inline tryGetKey (key: string) (json: JsonTree) =
        match json with
        | Object o -> OrderedMap.tryFind key o
        | _ -> None

    let inline tryGetKeyCaseInsensitive (key: string) (json: JsonTree) =
        match json with
        | Object o -> OrderedMap.tryFindCaseInsensitive key o
        | _ -> None

    let inline tryGetIndex (index: int) (json: JsonTree) =
        match json with
        | Array a ->
            if index >= 0 && index < a.Length then
                Some a.[index]
            else
                None
        | _ -> None

    let inline getKey (key: string) (json: JsonTree) =
        match json with
        | Object o -> o.[key]
        | _ -> failwithf "Expected object, got %A" json

    let inline getKeyCaseInsensitive (key: string) (json: JsonTree) =
        match json with
        | Object o -> OrderedMap.findCaseInsensitive key o
        | _ -> Null

    let inline getIndex (index: int) (json: JsonTree) =
        match json with
        | Array a -> a.[index]
        | _ -> failwithf "Expected array, got %A" json

    let inline getKeyOrNull (key: string) (json: JsonTree) =
        match json with
        | Object o ->
            match OrderedMap.tryFind key o with
            | Some v -> v
            | None -> Null
        | _ -> Null

    let inline getKeyCaseInsensitiveOrNull (key: string) (json: JsonTree) =
        match json with
        | Object o ->
            match OrderedMap.tryFindCaseInsensitive key o with
            | Some v -> v
            | None -> Null
        | _ -> Null

    let inline getIndexOrNull (index: int) (json: JsonTree) =
        match json with
        | Array a -> if index >= 0 && index < a.Length then a.[index] else Null
        | _ -> Null

    let inline getType (json: JsonTree) =
        match json with
        | Null -> JsonType.Null
        | Object _ -> JsonType.Object
        | Array _ -> JsonType.Array
        | String _ -> JsonType.String
        | Integer _ -> JsonType.Integer
        | Float _ -> JsonType.Float
        | Decimal _ -> JsonType.Decimal
        | Boolean _ -> JsonType.Boolean
