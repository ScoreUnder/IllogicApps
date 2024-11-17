module IllogicApps.Core.ExternalServiceTypes

open IllogicApps.Core.CompletedStepTypes
open IllogicApps.Json

type HttpRequest =
    { method: string
      uri: string
      headers: OrderedMap<string, string>
      queryParameters: OrderedMap<string, string>
      body: byte array option
      cookie: string option
      authentication: JsonTree }

type HttpTrigger =
    { queries: OrderedMap<string, string> option
      headers: OrderedMap<string, string> option
      body: JsonTree option }

let jsonOfHttpTrigger trigger =
    OrderedMap
        .Builder()
        .MaybeAdd("queries", trigger.queries |> Option.map Conversions.jsonOfStringsMap)
        .MaybeAdd("headers", trigger.headers |> Option.map Conversions.jsonOfStringsMap)
        .MaybeAdd("body", trigger.body)
        .Build()
    |> Object

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

type WorkflowRequestRetryPolicy =
    { type_: string
      count: int64 option
      interval: string option
      minimumInterval: string option
      maximumInterval: string option }

    static member Default =
        { type_ = "none"
          count = None
          interval = None
          minimumInterval = None
          maximumInterval = None }

let jsonOfWorkflowRequestRetryPolicy policy =
    OrderedMap
        .Builder()
        .Add("type", String policy.type_)
        .MaybeAdd("count", policy.count |> Option.map Integer)
        .MaybeAdd("interval", policy.interval)
        .MaybeAdd("minimumInterval", policy.minimumInterval)
        .MaybeAdd("maximumInterval", policy.maximumInterval)
        .Build()
    |> Object

let workflowRequestRetryPolicyOfJson json =
    { type_ = JsonTree.getKey "type" json |> Conversions.ensureString
      count = JsonTree.tryGetKey "count" json |> Option.map Conversions.ensureInteger
      interval = JsonTree.tryGetKey "interval" json |> Option.map Conversions.ensureString
      minimumInterval = JsonTree.tryGetKey "minimumInterval" json |> Option.map Conversions.ensureString
      maximumInterval = JsonTree.tryGetKey "maximumInterval" json |> Option.map Conversions.ensureString }

type WorkflowRequest =
    { actionName: string
      workflowId: string
      headers: OrderedMap<string, string>
      body: JsonTree
      asyncSupported: bool
      retryPolicy: WorkflowRequestRetryPolicy }

let jsonOfWorkflowRequest req =
    OrderedMap
        .Builder()
        .Add("host", Conversions.createObject [ "workflow", Conversions.createObject [ "id", String req.workflowId ] ])
        .Add("headers", req.headers |> OrderedMap.mapValuesOnly String |> Object)
        .MaybeAdd("body", req.body |> Conversions.optionOfJson)
        .Add("retryPolicy", jsonOfWorkflowRequestRetryPolicy req.retryPolicy)
        .Build()
    |> Object

let workflowRequestOfJson json =
    { actionName = ""
      workflowId =
        JsonTree.getKey "host" json
        |> JsonTree.getKey "workflow"
        |> JsonTree.getKey "id"
        |> Conversions.ensureString
      headers =
        JsonTree.tryGetKey "headers" json
        |> Option.map (fun headers ->
            headers
            |> Conversions.ensureObject
            // TODO: The designer should stringify all header values, but non-stringified ones
            // are still valid. Needs looking into re. specifics
            |> OrderedMap.mapValuesOnly Conversions.rawStringOfJson)
        |> Option.defaultValue OrderedMap.empty
      body = JsonTree.tryGetKey "body" json |> Conversions.jsonOfOption
      asyncSupported = true
      retryPolicy =
        JsonTree.tryGetKey "retryPolicy" json
        |> Option.map workflowRequestRetryPolicyOfJson
        |> Option.defaultValue WorkflowRequestRetryPolicy.Default }

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
    | HttpRequest of HttpRequest * HttpRequestReply ref
    | HttpResponse of HttpRequestReply
    | Workflow of WorkflowRequest * HttpRequestReply ref
    | ScriptExecution of ScriptExecutionRequest * Result<JsonTree, string> ref
    | ServiceProvider of ServiceProviderRequest * HttpRequestReply ref
