module IllogicApps.Core.ExternalServiceTypes

open System.Net.Http
open IllogicApps.Core.CompletedStepTypes
open IllogicApps.Core.HttpModel.RetryPolicy
open IllogicApps.Json

type HttpServiceRequest =
    { method: string
      uri: string
      headers: OrderedMap<string, string>
      queryParameters: OrderedMap<string, string>
      body: byte array option
      cookie: string option
      authentication: JsonTree }

type HttpRequest =
    { method: HttpMethod
      relativePath: string
      queries: OrderedMap<string, string> option
      headers: OrderedMap<string, string> option
      body: JsonTree option }

    static member Default =
        { method = HttpMethod.Post
          relativePath = ""
          queries = None
          headers = None
          body = None }

    member request.ToJson() =
        OrderedMap
            .Builder()
            .Add("method", String request.method.Method)
            .Add("relativePath", String request.relativePath)
            .MaybeAdd("queries", request.queries |> Option.map Conversions.jsonOfStringsMap)
            .MaybeAdd("headers", request.headers |> Option.map Conversions.jsonOfStringsMap)
            .MaybeAdd("body", request.body)
            .Build()
        |> Object

    override request.ToString() = request.ToJson().ToString()

let inline jsonOfHttpRequest (request: HttpRequest) = request.ToJson()

type HttpRequestReply =
    { statusCode: int
      headers: OrderedMap<string, string> option
      body: JsonTree option }

    static member Default =
        { statusCode = 0
          headers = None
          body = None }

let jsonOfHttpRequestReply reply =
    OrderedMap
        .Builder()
        .Add("statusCode", Integer reply.statusCode)
        .MaybeAdd("headers", reply.headers |> Option.map Conversions.jsonOfStringsMap)
        .MaybeAdd("body", reply.body)
        .Build()
    |> Object

/// A request to invoke a workflow from an action in this workflow
type WorkflowRequest =
    {
        /// Name of the action which sent the request
        /// Set on execution; not present in the workflow json
        actionName: string
        /// Name of the workflow to be invoked
        workflowId: string
        /// HTTP headers to be sent with the request
        headers: OrderedMap<string, string>
        /// Request body (not serialized)
        body: JsonTree
        /// Whether async processing is supported (i.e. HTTP 201 + polling)
        /// Set on execution; not present in the workflow json
        asyncSupported: bool
        /// Retry policy for the request
        /// (Note that this is manual on the receiver's end, unlike with
        /// HTTP for example)
        retryPolicy: RetryPolicy option
    }

let jsonOfWorkflowRequest req =
    OrderedMap
        .Builder()
        .Add("actionName", String req.actionName)
        .Add("host", Conversions.createObject [ "workflow", Conversions.createObject [ "id", String req.workflowId ] ])
        .Add("headers", req.headers |> OrderedMap.mapValuesOnly String |> Object)
        .MaybeAdd("body", req.body |> Conversions.optionOfJson)
        .Add("asyncSupported", Boolean req.asyncSupported)
        .MaybeAdd("retryPolicy", req.retryPolicy |> Option.map jsonOfRetryPolicy)
        .Build()
    |> Object

let workflowRequestOfJson json =
    { actionName = JsonTree.getKeyMapOrElse "actionName" Conversions.ensureString (fun () -> "") json
      workflowId =
        JsonTree.getKey "host" json
        |> JsonTree.getKey "workflow"
        |> JsonTree.getKey "id"
        |> Conversions.ensureString
      headers =
        JsonTree.getKeyMapOrElse
            "headers"
            (fun headers ->
                headers
                |> Conversions.ensureObject
                // TODO: The designer should stringify all header values, but non-stringified ones
                // are still valid. Needs looking into re. specifics
                |> OrderedMap.mapValuesOnly Conversions.rawStringOfJson)
            (fun () -> OrderedMap.empty)
            json
      body = JsonTree.getKeyOrNull "body" json
      asyncSupported = JsonTree.getKeyMapOrElse "asyncSupported" Conversions.ensureBoolean (fun () -> true) json
      retryPolicy = JsonTree.tryGetKey "retryPolicy" json |> Option.map retryPolicyOfJson }

type WorkflowRunDetails =
    { id: string
      name: string
      type_: string }

    static member Create workflowId runName =
        { id = $"/workflows/{workflowId}/runs/{runName}"
          name = runName
          type_ = "workflows/runs" }

let jsonOfWorkflowRunDetails run =
    OrderedMap
        .Builder()
        .Add("id", String run.id)
        .Add("name", String run.name)
        .Add("type", String run.type_)
        .Build()
    |> Object

type WorkflowDetails =
    { id: string
      name: string
      type_: string
      run: WorkflowRunDetails }

    static member Create workflowId workflowName runName =
        { id = $"/workflows/{workflowId}"
          name = workflowName
          type_ = "workflows"
          run = WorkflowRunDetails.Create workflowId runName }

let jsonOfWorkflowDetails details =
    OrderedMap
        .Builder()
        .Add("id", String details.id)
        .Add("name", String details.name)
        .Add("type", String details.type_)
        .Add("run", jsonOfWorkflowRunDetails details.run)
        .Build()
    |> Object

type ScriptSource =
    | Inline of string
    | File of string

type ScriptingLanguage =
    | JavaScript
    | CSharp
    | PowerShell

type ScriptExecutionRequest =
    { source: ScriptSource
      language: ScriptingLanguage
      actions: OrderedMap<string, CompletedAction>
      workflow: WorkflowDetails
      trigger: CompletedTrigger }

type ServiceProviderConfiguration =
    { connectionName: string
      operationId: string
      serviceProviderId: string }

let serviceProviderConfigurationOfJson json =
    { connectionName = JsonTree.getKey "connectionName" json |> Conversions.ensureString
      operationId = JsonTree.getKey "operationId" json |> Conversions.ensureString
      serviceProviderId = JsonTree.getKey "serviceProviderId" json |> Conversions.ensureString }

let jsonOfServiceProviderConfiguration (config: ServiceProviderConfiguration) =
    OrderedMap
        .Builder()
        .Add("connectionName", JsonTree.String config.connectionName)
        .Add("operationId", JsonTree.String config.operationId)
        .Add("serviceProviderId", JsonTree.String config.serviceProviderId)
        .Build()
    |> JsonTree.Object

type ServiceProviderRequest =
    { actionName: string
      parameters: JsonTree
      serviceProviderConfiguration: ServiceProviderConfiguration }

type ExternalServiceRequest =
    | HttpRequest of HttpServiceRequest * HttpRequestReply ref
    | HttpResponse of HttpRequestReply
    | Workflow of WorkflowRequest * HttpRequestReply ref
    | ScriptExecution of ScriptExecutionRequest * Result<JsonTree, string> ref
    | ServiceProvider of ServiceProviderRequest * HttpRequestReply ref
