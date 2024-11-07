module IllogicApps.Core.ExternalServiceTypeConversions

open System
open System.Net
open CompletedStepTypes
open ExternalServiceTypes
open IllogicApps.Json

let formUri (str: string) (query: Map<string, string>) =
    let uriBuilder = UriBuilder(str)

    let query =
        query
        |> Map.fold (fun acc k v -> "&" :: WebUtility.UrlEncode(k) :: "=" :: WebUtility.UrlEncode(v) :: acc) []

    let query =
        match query with
        | [] -> ""
        | _ :: parts -> String.concat "" ("?" :: parts)

    uriBuilder.Query <- query
    uriBuilder.Uri

let decodeBodyByContentType (contentType: string) (body: string) =
    if contentType.Equals("application/json", StringComparison.OrdinalIgnoreCase) then
        Parser.parse body
    else
        String body

let netHttpRequestMessageOfHttpRequest (req: HttpRequest) =
    let netReq =
        new Http.HttpRequestMessage(
            method = Http.HttpMethod(req.method),
            requestUri = formUri req.uri req.queryParameters
        )

    match req.body with
    | Some content -> netReq.Content <- new Http.StringContent(content)
    | None -> ()

    req.cookie |> Option.iter (fun v -> netReq.Headers.Add("Cookie", v))
    req.headers |> Map.iter (fun k v -> netReq.Headers.Add(k, v))

    // Not implemented, probably shouldn't be handled here: authentication

    netReq

let httpRequestReplyOfNetHttpResponseMessage (resp: Http.HttpResponseMessage) =
    let headers =
        resp.Headers
        // TODO: this doesn't handle multiple headers with the same key
        // (and nor do my types in ExternalServiceTypes)
        |> Seq.collect (fun kvp -> kvp.Value |> Seq.map (fun v -> (kvp.Key, v)))
        |> Map.ofSeq

    let body =
        resp.Content.ReadAsStringAsync().Result
        |> decodeBodyByContentType resp.Content.Headers.ContentType.MediaType

    { statusCode = int resp.StatusCode
      headers = headers
      body = body }

let netHttpRequestMessageOfWorkflowRequest (req: WorkflowRequest) =
    let netReq =
        new Http.HttpRequestMessage(
            method = Http.HttpMethod("POST"),
            requestUri = Uri($"https://dummyWorkflowInvoker/{req.workflowId}")
        )

    match req.body with
    | Null -> ()
    | String s ->
        netReq.Headers.Add("Content-Type", "text/plain")
        netReq.Content <- new Http.StringContent(s)
    // TODO: do array/number/etc get handled here?
    | _ ->
        netReq.Headers.Add("Content-Type", "application/json")
        netReq.Content <- new Http.StringContent(req.body |> Conversions.stringOfJson)

    req.headers |> Map.iter (fun k v -> netReq.Headers.Add(k, v))

    // Not implemented, probably shouldn't be handled here: retryPolicy

    netReq

let completedTriggerOfWorkflowRequest (req: WorkflowRequest) =
    CompletedTrigger.create
    <| { CompletedAction.create req.workflowId (stringOfDateTime DateTime.UtcNow) with
           outputs = req.body }
