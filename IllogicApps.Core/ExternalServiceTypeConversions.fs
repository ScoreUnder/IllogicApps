module IllogicApps.Core.ExternalServiceTypeConversions

open System
open System.Net
open System.Net.Http.Headers
open ExternalServiceTypes
open IllogicApps.Core.HttpModel.HttpParsing
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

let netHttpContentOfContentTypeAndContent =
    function
    | None -> new Http.ByteArrayContent(Array.empty)
    | Some(contentType, content) ->
        let content = new Http.ByteArrayContent(content)
        content.Headers.ContentType <- MediaTypeHeaderValue.Parse(contentType)
        content

let netHttpContentOfJson json =
    json |> contentOfJson |> netHttpContentOfContentTypeAndContent

let netHttpRequestMessageOfHttpServiceRequest (req: HttpServiceRequest) =
    let netReq =
        new Http.HttpRequestMessage(
            method = Http.HttpMethod(req.method),
            requestUri = formUri req.uri req.queryParameters
        )

    netReq.Content <-
        req.body
        |> Option.map (fun b ->
            let contentType =
                OrderedMap.tryFindCaseInsensitive "Content-Type" req.headers
                |> Option.defaultValue ContentType.TextUtf8

            contentType, b)
        |> netHttpContentOfContentTypeAndContent

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
            let contentType = content.Headers.ContentType |> Option.ofObj |> Option.map string

            content.ReadAsByteArrayAsync().GetAwaiter().GetResult()
            |> decodeOptionalBodyByContentType contentType

    { statusCode = int resp.StatusCode
      headers = Some headers
      body = body }

let netHttpRequestMessageOfWorkflowRequest (req: WorkflowRequest) =
    let netReq =
        new Http.HttpRequestMessage(
            method = Http.HttpMethod("POST"),
            requestUri = Uri($"https://dummyWorkflowInvoker/{req.workflowId}")
        )

    netReq.Content <- netHttpContentOfJson req.body

    req.headers
    |> OrderedMap.iter (fun k v ->
        netReq.Headers.TryAddWithoutValidation(k, Conversions.rawStringOfJson v)
        |> ignore)

    // Not implemented, probably shouldn't be handled here: retryPolicy

    netReq

let netHttpResponseMessageOfHttpRequestReply (resp: HttpRequestReply) =
    let netResp =
        new Http.HttpResponseMessage(
            enum<HttpStatusCode> resp.statusCode,
            Content = (resp.body |> Conversions.jsonOfOption |> netHttpContentOfJson)
        )

    resp.headers
    |> Option.iter (OrderedMap.iter (fun k v -> netResp.Headers.TryAddWithoutValidation(k, v) |> ignore))

    netResp

let httpStatusCodeIsSuccess (code: int) = code >= 200 && code < 300
