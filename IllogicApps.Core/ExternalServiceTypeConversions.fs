module IllogicApps.Core.ExternalServiceTypeConversions

open System
open System.Net
open System.Net.Http.Headers
open System.Text
open ExternalServiceTypes
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

let decodeBodyByContentType (contentType: string) (body: string) =
    if ContentType.isJson contentType then Parser.parse body
    elif ContentType.isAnyText contentType then String body
    else Blob.ofString contentType body

let contentTypeOfJsonType (json: JsonType) =
    match json with
    | JsonType.Null -> None
    | JsonType.String -> Some "text/plain"
    | _ -> Some "application/json"

let netHttpRequestMessageOfHttpRequest (req: HttpRequest) =
    let netReq =
        new Http.HttpRequestMessage(
            method = Http.HttpMethod(req.method),
            requestUri = formUri req.uri req.queryParameters
        )

    match req.body with
    | Some content ->
        let contentType =
            OrderedMap.tryFindCaseInsensitive "Content-Type" req.headers
            |> Option.defaultValue "text/plain"

        netReq.Content <- new Http.StringContent(content, Encoding.UTF8, contentType)
    | None -> ()

    req.cookie
    |> Option.iter (fun v -> netReq.Headers.TryAddWithoutValidation("Cookie", v) |> ignore)

    req.headers
    |> OrderedMap.iter (fun k v -> netReq.Headers.TryAddWithoutValidation(k, v) |> ignore)

    // Not implemented, probably shouldn't be handled here: authentication

    netReq

let httpRequestReplyOfNetHttpResponseMessage (resp: Http.HttpResponseMessage) =
    let headers =
        Seq.concat [ resp.Headers :> HttpHeaders; resp.Content.Headers; resp.TrailingHeaders ]
        |> Seq.map (fun (KeyValue(k, v)) -> k, String.concat "," v)
        |> OrderedMap.ofSeq

    let body =
        match resp.Content with
        | null -> None
        | content ->
            let contentType =
                content.Headers.ContentType
                |> Option.ofObj
                |> Option.bind (fun o -> o.MediaType |> Option.ofObj)

            let contentStr = content.ReadAsStringAsync().Result

            if String.IsNullOrEmpty contentStr && Option.isNone contentType then
                None
            else
                contentStr
                |> decodeBodyByContentType (Option.defaultValue "application/octet-stream" contentType)
                |> Some

    { statusCode = int resp.StatusCode
      headers = Some headers
      body = body }

let netHttpRequestMessageOfWorkflowRequest (req: WorkflowRequest) =
    let netReq =
        new Http.HttpRequestMessage(
            method = Http.HttpMethod("POST"),
            requestUri = Uri($"https://dummyWorkflowInvoker/{req.workflowId}")
        )

    match req.body |> JsonTree.getType |> contentTypeOfJsonType with
    | None -> ()
    | Some typ -> netReq.Content <- new Http.StringContent(req.body |> Conversions.stringOfJson, Encoding.UTF8, typ)

    req.headers
    |> OrderedMap.iter (fun k v -> netReq.Headers.TryAddWithoutValidation(k, v) |> ignore)

    // Not implemented, probably shouldn't be handled here: retryPolicy

    netReq

let netHttpResponseMessageOfHttpRequestReply (resp: HttpRequestReply) =
    let netResp =
        new Http.HttpResponseMessage(
            enum<HttpStatusCode> resp.statusCode,
            Content =
                (resp.body
                 |> Option.bind (fun body ->
                     match contentTypeOfJsonType (JsonTree.getType body) with
                     | Some type_ ->
                         Some(new Http.StringContent(body |> Conversions.rawStringOfJson, Encoding.UTF8, type_))
                     | None -> None)
                 |> Option.toObj)
        )

    resp.headers
    |> Option.iter (OrderedMap.iter (fun k v -> netResp.Headers.TryAddWithoutValidation(k, v) |> ignore))

    netResp

let httpStatusCodeIsSuccess (code: int) = code >= 200 && code < 300
