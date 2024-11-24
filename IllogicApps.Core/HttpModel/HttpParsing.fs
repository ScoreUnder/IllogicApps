module IllogicApps.Core.HttpModel.HttpParsing

open System.Net
open System.Text
open IllogicApps.Core.Support
open IllogicApps.Json

let parseQueryString =
    function
    | null
    | ""
    | "?" -> OrderedMap.empty
    | query ->
        let query = if query.StartsWith("?") then query.Substring(1) else query

        query.Split('&')
        |> Seq.map (fun part ->
            let parts = part.Split('=', 2)

            match parts with
            | [| key; value |] -> WebUtility.UrlDecode(key), WebUtility.UrlDecode(value)
            | [| value |] -> null, WebUtility.UrlDecode(value)
            | _ -> failwith "Should never happen")
        |> Seq.groupBy fst
        |> Seq.map (fun (key, values) -> key, (values |> Seq.map snd |> String.concat ","))
        |> OrderedMap.ofSeq

let decodeBodyByContentType (contentType: string) (body: byte array) =
    let decodeBody () =
        let charset =
            ContentType.Charset.get contentType |> Option.defaultValue Encoding.UTF8

        charset.GetString(body)

    if ContentType.isJson contentType then
        Parser.parse (decodeBody ())
    elif ContentType.isAnyText contentType then
        String(decodeBody ())
    else
        Blob.ofBytes contentType body

let decodeOptionalBodyByContentType (contentType: string option) (body: byte array) =
    if body.Length = 0 && Option.isNone contentType then
        None
    else
        let contentType = Option.defaultValue ContentType.Binary contentType
        Some(decodeBodyByContentType contentType body)

let contentOfJson =
    function
    | Null -> None
    | String s -> Some(ContentType.TextUtf8, s |> Encoding.UTF8.GetBytes)
    | Blob.Blob(contentType, content) -> Some(contentType, content)
    | json -> Some(ContentType.JsonUtf8, json |> Conversions.stringOfJson |> Encoding.UTF8.GetBytes)
