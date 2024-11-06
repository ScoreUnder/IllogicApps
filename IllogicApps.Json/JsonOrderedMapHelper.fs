namespace IllogicApps.Json

open System.Runtime.CompilerServices

type OrderedMapBuilderExtensions =
    [<Extension>]
    static member MaybeAdd(map: OrderedMap.Builder<string, JsonTree>, key: string, value: JsonTree) =
        match value with
        | Null -> map
        | _ -> map.Add(key, value)

    [<Extension>]
    static member MaybeAdd(map: OrderedMap.Builder<string, JsonTree>, key: string, value: string option) =
        match value with
        | None -> map
        | Some s -> map.Add(key, String s)
