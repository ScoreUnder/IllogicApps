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