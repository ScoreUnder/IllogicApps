module IllogicApps.Core.HttpModel.HttpWriting

open System
open System.Net
open System.Net.Http.Headers
open System.Text

open IllogicApps.Core.Support
open IllogicApps.Json

let formUri (str: string) (query: OrderedMap<string, string>) =
    let uriBuilder = UriBuilder(str)

    let query =
        query
        |> OrderedMap.fold (fun acc k v -> "&" :: WebUtility.UrlEncode(k) :: "=" :: WebUtility.UrlEncode(v) :: acc) []

    let query =
        match query with
        | [] -> ""
        | _ :: parts -> String.concat "" ("?" :: parts)

    uriBuilder.Query <- query
    uriBuilder.Uri

let contentOfJson =
    function
    | Null -> None
    | String s -> Some(ContentType.TextUtf8, s |> Encoding.UTF8.GetBytes)
    | Blob.Blob(contentType, content) -> Some(contentType, content)
    | json -> Some(ContentType.JsonUtf8, json |> Conversions.stringOfJson |> Encoding.UTF8.GetBytes)

let netHttpContentOfContentTypeAndContent =
    function
    | None -> new Http.ByteArrayContent(Array.empty)
    | Some(contentType, content) ->
        let content = new Http.ByteArrayContent(content)
        content.Headers.ContentType <- MediaTypeHeaderValue.Parse(contentType)
        content

let netHttpContentOfJson json =
    json |> contentOfJson |> netHttpContentOfContentTypeAndContent
