module internal IllogicApps.Compat.LogicAppUnit.InternalHelper

open IllogicApps.Json

let inline sanitiseNull (def: 'a) (value: 'a) = if value = null then def else value

let parseQueryString =
    function
    | null
    | ""
    | "?" -> OrderedMap.empty
    | query ->
        query.Split('&')
        |> Seq.map (fun part ->
            let parts = part.Split('=', 2)

            match parts with
            | [| key; value |] -> key, value
            | [| value |] -> null, value
            | _ -> failwith "Should never happen")
        |> Seq.groupBy fst
        |> Seq.map (fun (key, values) -> key, (values |> Seq.map snd |> String.concat ","))
        |> OrderedMap.ofSeq
