namespace IllogicApps.Json

open System.Runtime.CompilerServices

type OrderedMapBuilderExtensions =
    [<Extension>]
    static member MaybeAdd(map: OrderedMap.Builder<string, JsonTree>, key: string, value: JsonTree) =
        match value with
        | Null -> map
        | _ -> map.Add(key, value)

    [<Extension>]
    static member MaybeAdd(map: OrderedMap.Builder<string, JsonTree>, key: string, value: JsonTree option) =
        match value with
        | None -> map
        | Some value -> map.Add(key, value)

    [<Extension>]
    static member MaybeAdd(map: OrderedMap.Builder<string, JsonTree>, key: string, value: string option) =
        match value with
        | None -> map
        | Some s -> map.Add(key, String s)

    [<Extension>]
    static member MaybeAdd
        (map: OrderedMap.Builder<string, JsonTree>, key: string, value: OrderedMap<string, string> option)
        =
        match value with
        | None -> map
        | Some value -> map.Add(key, Conversions.jsonOfStringsMap value)

type OrderedMapExtensions =
    [<Extension>]
    static member TryAdd(map: OrderedMap<string, 'V>, key: string, value: 'V, comparison: System.StringComparer) =
        if map.Keys.Contains(key, comparison) then
            map
        else
            OrderedMap.unsafeAdd key value map
