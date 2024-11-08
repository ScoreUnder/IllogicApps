module IllogicApps.Core.ExternalServiceTypes

open System.Collections.Generic
open IllogicApps.Core.CompletedStepTypes
open IllogicApps.Json

type HttpRequest =
    { method: string
      uri: string
      headers: OrderedMap<string, string>
      queryParameters: OrderedMap<string, string>
      body: string option
      cookie: string option
      authentication: JsonTree }

type HttpRequestReply =
    { statusCode: int
      headers: OrderedMap<string, string>
      body: JsonTree }

    static member Default =
        { statusCode = 0
          headers = OrderedMap.empty
          body = Null }

let jsonOfHttpRequestReply reply =
    OrderedMap
        .Builder()
        .Add("statusCode", Integer reply.statusCode)
        .Add("headers", reply.headers |> Conversions.jsonOfStringsMap)
        .MaybeAdd("body", reply.body)
        .Build()
    |> Object

type WorkflowRequestRetryPolicy =
    { type_: string
      count: int64
      interval: string
      minimumInterval: string
      maximumInterval: string }

    static member Default =
        { type_ = ""
          count = 0
          interval = ""
          minimumInterval = ""
          maximumInterval = "" }

let jsonOfWorkflowRequestRetryPolicy policy =
    OrderedMap
        .Builder()
        .Add("type", String policy.type_)
        .Add("count", Integer policy.count)
        .Add("interval", String policy.interval)
        .Add("minimumInterval", String policy.minimumInterval)
        .Add("maximumInterval", String policy.maximumInterval)
        .Build()
    |> Object

let workflowRequestRetryPolicyOfJson json =
    { type_ = JsonTree.getKey "type" json |> Conversions.ensureString
      count = JsonTree.getKey "count" json |> Conversions.ensureInteger
      interval = JsonTree.getKey "interval" json |> Conversions.ensureString
      minimumInterval = JsonTree.getKey "minimumInterval" json |> Conversions.ensureString
      maximumInterval = JsonTree.getKey "maximumInterval" json |> Conversions.ensureString }

type WorkflowRequest =
    { workflowId: string
      headers: OrderedMap<string, string>
      body: JsonTree
      retryPolicy: WorkflowRequestRetryPolicy }

let jsonOfWorkflowRequest req =
    OrderedMap
        .Builder()
        .Add("host", Conversions.createObject [ "workflow", Conversions.createObject [ "id", String req.workflowId ] ])
        .Add(
            "headers",
            req.headers
            |> Seq.map (fun (KeyValue(k, v)) -> KeyValuePair(k, String v))
            |> OrderedMap.CreateRange
            |> Object
        )
        .Add("body", req.body)
        .Add("retryPolicy", jsonOfWorkflowRequestRetryPolicy req.retryPolicy)
        .Build()
    |> Object

let workflowRequestOfJson json =
    { workflowId =
        JsonTree.getKey "host" json
        |> JsonTree.getKey "workflow"
        |> JsonTree.getKey "id"
        |> Conversions.ensureString
      headers =
        JsonTree.getKey "headers" json
        |> Conversions.ensureObject
        |> OrderedMap.mapValuesOnly Conversions.ensureString
      body = JsonTree.getKey "body" json
      retryPolicy = JsonTree.getKey "retryPolicy" json |> workflowRequestRetryPolicyOfJson }

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

type ExternalServiceRequestType =
    | HttpRequest of HttpRequest * HttpRequestReply ref
    | HttpResponse of HttpRequestReply
    | Workflow of WorkflowRequest * HttpRequestReply ref
    | ScriptExecution of ScriptExecutionRequest * Result<JsonTree, string> ref
