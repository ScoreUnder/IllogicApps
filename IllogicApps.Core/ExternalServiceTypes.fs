module IllogicApps.Core.ExternalServiceTypes

open System.Collections.Generic
open IllogicApps.Json

type HttpRequest =
    { method: string
      uri: string
      headers: Map<string, string>
      queryParameters: Map<string, string>
      body: string option
      cookie: string option
      authentication: JsonTree }

type HttpRequestReply =
    { statusCode: int
      headers: Map<string, string>
      body: JsonTree }
    
    static member Default =
        { statusCode = 0
          headers = Map.empty
          body = Null }

type WorkflowRequestRetryPolicy =
    { type_: string
      count: int
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

type WorkflowRequest =
    { workflowId: string
      headers: Map<string, string>
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


type ExternalServiceRequestType =
    | HttpRequest of HttpRequest * HttpRequestReply ref
    | HttpResponse of HttpRequestReply
    | Workflow of WorkflowRequest * HttpRequestReply ref
