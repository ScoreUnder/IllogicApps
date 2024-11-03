module IllogicApps.Core.ExternalServiceTypeConversions

open System
open System.Net
open System.Text.Json
open System.Text.Json.Nodes
open CompletedStepTypes
open ExternalServiceTypes

let formUri (str: string) (query: Map<string, string>) =
    let uriBuilder = new UriBuilder(str)

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
        JsonNode.Parse body
    else
        JsonValue.Create body

let netHttpRequestMessageOfHttpRequest (req: HttpRequest) =
    let netReq =
        new Http.HttpRequestMessage(
            method = new Http.HttpMethod(req.Method),
            requestUri = formUri req.Uri req.QueryParameters,
            Content = new Http.StringContent(req.Body.Value))

    req.Cookie |> Option.iter (fun v -> netReq.Headers.Add("Cookie", v))
    req.Headers |> Map.iter (fun k v -> netReq.Headers.Add(k, v))

    // Not implemented, probably shouldn't be handled here: Authentication

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
        |> decodeBodyByContentType (resp.Content.Headers.ContentType.MediaType)

    // Not implemented, probably shouldn't be handled here: RetryPolicy

    new HttpRequestReply(
        StatusCode = (int) resp.StatusCode,
        Headers = headers,
        Body = body)

let netHttpRequestMessageOfWorkflowRequest (req: WorkflowRequest) =
    let netReq =
        new Http.HttpRequestMessage(
            method = new Http.HttpMethod("POST"),
            requestUri = new Uri($"https://dummyWorkflowInvoker/{req.Host.Workflow.Id}"),
            Content = new Http.StringContent(req.Body.ToString()))

    if req.Body.GetValueKind() = JsonValueKind.Object then
        netReq.Headers.Add("Content-Type", "application/json")
    else
        netReq.Headers.Add("Content-Type", "text/plain")

    req.Headers |> Map.iter (fun k v -> netReq.Headers.Add(k, v))

    // Not implemented, probably shouldn't be handled here: Host, RetryPolicy

    netReq

let completedTriggerOfWorkflowRequest (req: WorkflowRequest) =
    CompletedTrigger(
        name = req.Host.Workflow.Id,
        status = Succeeded,
        startTime = stringOfDateTime DateTime.UtcNow,
        EndTime = stringOfDateTime DateTime.UtcNow,
        Outputs = JsonUtil.illogicJsonOfSystemTextJson req.Body
    )
