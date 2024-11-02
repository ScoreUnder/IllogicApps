[<AutoOpen>]
module IllogicApps.Json.CoreTypes

open System.Collections.Immutable

type JsonTree =
    | Null
    | Object of Map<string, JsonTree>
    | Array of JsonTree ImmutableArray
    | String of string
    | Integer of int64
    | Float of float
    | Decimal of decimal
    | Boolean of bool

module JsonTree =
    let inline tryGetKey (key: string) (json: JsonTree) =
        match json with
        | Object o -> o.TryFind key
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

    let inline getIndex (index: int) (json: JsonTree) =
        match json with
        | Array a -> a.[index]
        | _ -> failwithf "Expected array, got %A" json
