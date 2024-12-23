module IllogicApps.Core.LogicAppActionSupport

open System
open IllogicApps.Core.CompletedStepTypes
open IllogicApps.Core.ExternalServiceTypes
open IllogicApps.Core.LogicAppSpec
open IllogicApps.Json

type HttpRequestTriggerInputs =
    { method: string option
      relativePath: string option }

    member this.ToJson() =
        OrderedMap
            .Builder()
            .MaybeAdd("method", this.method)
            .MaybeAdd("relativePath", this.relativePath)
            .Build()
        |> Object

    override this.ToString() = this.ToJson().ToString()

let httpRequestTriggerInputsOfJson json =
    { method = JsonTree.tryGetKey "method" json |> Option.map Conversions.ensureString
      relativePath = JsonTree.tryGetKey "relativePath" json |> Option.map Conversions.ensureString }

type HttpRequestTriggerOutputs =
    { headers: OrderedMap<string, string> option
      queries: OrderedMap<string, string> option
      relativePathParameters: OrderedMap<string, string> option
      body: JsonTree option }

    member this.ToJson() =
        OrderedMap
            .Builder()
            .MaybeAdd("headers", this.headers)
            .MaybeAdd("queries", this.queries)
            .MaybeAdd("relativePathParameters", this.relativePathParameters)
            .MaybeAdd("body", this.body)
            .Build()
        |> Object

    override this.ToString() = this.ToJson().ToString()

type SetVariableSingle = { name: string; value: JsonTree }

let setVariableSingleOfJson json =
    { name = JsonTree.getKey "name" json |> Conversions.ensureString
      value = JsonTree.getKey "value" json }

type VariableType =
    | String
    | Integer
    | Float
    | Boolean
    | Object
    | Array

    override this.ToString() =
        match this with
        | String -> "String"
        | Integer -> "Integer"
        | Float -> "Float"
        | Boolean -> "Boolean"
        | Object -> "Object"
        | Array -> "Array"

let variableTypeOfString (s: string) =
    match s.ToLowerInvariant() with
    | "string" -> VariableType.String
    | "integer" -> VariableType.Integer
    | "float" -> VariableType.Float
    | "boolean" -> VariableType.Boolean
    | "object" -> VariableType.Object
    | "array" -> VariableType.Array
    | _ -> failwithf "Unknown variable type: %s" s

let variableTypeOfJson (json: JsonTree) =
    json |> Conversions.ensureString |> variableTypeOfString

type InitializeVariableSingle =
    { name: string
      value: JsonTree
      type_: VariableType }

let initializeVariableSingleOfJson json =
    { name = JsonTree.getKey "name" json |> Conversions.ensureString
      value = JsonTree.getKeyOrNull "value" json
      type_ = JsonTree.getKey "type" json |> variableTypeOfJson }

type 'a VariablesInputs = { variables: 'a list }

let variablesInputsOfJson elOfJson json =
    { variables =
        JsonTree.getKey "variables" json
        |> Conversions.ensureArray
        |> Seq.map elOfJson
        |> List.ofSeq }

type ParseJsonInputs =
    { content: JsonTree
      schema: JsonTree }

    override this.ToString() =
        $"{{{nameof ParseJsonInputs}.{nameof this.content}={this.content}; {nameof this.schema}={this.schema}}}"

let parseJsonInputsOfJson json =
    { content = JsonTree.getKey "content" json
      schema = JsonTree.getKey "schema" json }

type QueryInputs = { from: JsonTree; where: JsonTree }

let queryInputsOfJson json =
    { from = JsonTree.getKey "from" json
      where = JsonTree.getKey "where" json }

type SelectInputs = { from: JsonTree; select: JsonTree }

let selectInputsOfJson json =
    { from = JsonTree.getKey "from" json
      select = JsonTree.getKey "select" json }

type JavaScriptCodeDependencies =
    { includeTrigger: bool option
      actions: string list option }

let javaScriptCodeDependenciesOfJson json =
    { includeTrigger = JsonTree.tryGetKey "includeTrigger" json |> Option.map Conversions.ensureBoolean
      actions =
        JsonTree.tryGetKey "actions" json
        |> Option.map (fun v -> v |> Conversions.ensureArray |> Seq.map Conversions.ensureString |> List.ofSeq) }

let jsonOfJavaScriptCodeDependencies (deps: JavaScriptCodeDependencies) =
    OrderedMap
        .Builder()
        .MaybeAdd("includeTrigger", deps.includeTrigger |> Option.map JsonTree.Boolean)
        .MaybeAdd("actions", deps.actions |> Option.map Conversions.jsonOfStringList)
        .Build()
    |> JsonTree.Object

type JavaScriptCodeInputs =
    { code: string
      explicitDependencies: JavaScriptCodeDependencies option }

    member inputs.ToJson() =
        OrderedMap
            .Builder()
            .Add("code", JsonTree.String inputs.code)
            .MaybeAdd(
                "explicitDependencies",
                inputs.explicitDependencies |> Option.map jsonOfJavaScriptCodeDependencies
            )
            .Build()
        |> JsonTree.Object

    override this.ToString() = this.ToJson().ToString()

let javaScriptCodeInputsOfJson json =
    { code = JsonTree.getKey "code" json |> Conversions.ensureString
      explicitDependencies =
        JsonTree.tryGetKey "explicitDependencies" json
        |> Option.map javaScriptCodeDependenciesOfJson }

let inline jsonOfJavaScriptCodeInputs (inputs: JavaScriptCodeInputs) = inputs.ToJson()

type ServiceProviderInputs =
    { parameters: JsonTree
      serviceProviderConfiguration: ServiceProviderConfiguration }

    member inputs.ToJson() =
        OrderedMap
            .Builder()
            .MaybeAdd("parameters", inputs.parameters)
            .Add("serviceProviderConfiguration", jsonOfServiceProviderConfiguration inputs.serviceProviderConfiguration)
            .Build()
        |> JsonTree.Object

    override this.ToString() = this.ToJson().ToString()

let serviceProviderInputsOfJson json =
    { parameters = JsonTree.getKeyOrNull "parameters" json
      serviceProviderConfiguration =
        JsonTree.getKey "serviceProviderConfiguration" json
        |> serviceProviderConfigurationOfJson }

let jsonOfServiceProviderInputs (inputs: ServiceProviderInputs) =
    OrderedMap
        .Builder()
        .MaybeAdd("parameters", inputs.parameters)
        .Add("serviceProviderConfiguration", jsonOfServiceProviderConfiguration inputs.serviceProviderConfiguration)
        .Build()
    |> JsonTree.Object

type InvokeFunctionInputs =
    { functionName: string
      parameters: JsonTree }

    member inputs.ToJson() =
        OrderedMap
            .Builder()
            .Add("functionName", JsonTree.String inputs.functionName)
            .Add("parameters", inputs.parameters)
            .Build()
        |> JsonTree.Object

    [<CompiledName("OfJson")>]
    static member ofJson json =
        { functionName = JsonTree.getKey "functionName" json |> Conversions.ensureString
          parameters = JsonTree.getKey "parameters" json }

    override this.ToString() = this.ToJson().ToString()

type HttpResponseInputs =
    { body: JsonTree
      headers: OrderedMap<string, string> option
      statusCode: JsonTree }

    member inputs.ToJson() =
        OrderedMap
            .Builder()
            .Add("statusCode", inputs.statusCode)
            .MaybeAdd("headers", inputs.headers)
            .MaybeAdd("body", inputs.body)
            .Build()
        |> JsonTree.Object

    override this.ToString() = this.ToJson().ToString()

let inline jsonOfHttpResponseInputs (inputs: HttpResponseInputs) = inputs.ToJson()

let httpResponseInputsOfJson json =
    { body = JsonTree.getKeyOrNull "body" json
      headers = JsonTree.tryGetKey "headers" json |> Option.map Conversions.stringsMapOfJson
      statusCode = JsonTree.getKey "statusCode" json }

let makeWorkflowHttpRequestHeaders (actionName: string) (sim: SimulatorContext) =
    let workflowDetails = sim.WorkflowDetails
    let trigger = sim.TriggerResult
    let correlationId = trigger.trackingId
    let actionTrackingId = Guid.NewGuid().ToString() // TODO: use real action tracking ID
    let clientRequestId = Guid.NewGuid().ToString() // TODO: what is this?
    let runTrackingId = Guid.NewGuid().ToString() // TODO: what is this?

    [ ("x-ms-workflow-id", workflowDetails.id.Split('/') |> Array.last)
      ("x-ms-workflow-version", workflowDetails.version)
      ("x-ms-workflow-name", workflowDetails.name)
      ("x-ms-workflow-system-id", $"/scaleunits/prod-00{workflowDetails.id}")
      ("x-ms-workflow-run-id", workflowDetails.run.name)
      ("x-ms-workflow-run-tracking-id", runTrackingId)
      ("x-ms-workflow-operation-name", actionName)
      ("x-ms-tracking-id", correlationId)
      ("x-ms-correlation-id", correlationId)
      ("x-ms-client-request-id", clientRequestId)
      ("x-ms-client-tracking-id", trigger.clientTrackingId)
      ("x-ms-action-tracking-id", actionTrackingId)
      ("x-ms-activity-vector", "IN.04") ] // TODO: what is this?

let addWorkflowHttpRequestHeadersToBuilder
    (actionName: string)
    (sim: SimulatorContext)
    (headers: OrderedMap.Builder<string, string>)
    =
    makeWorkflowHttpRequestHeaders actionName sim
    |> List.iter (fun (k, v) -> headers.TryAdd(k, v) |> ignore)

    headers

let addWorkflowResponseHeadersToBuilder (sim: SimulatorContext) (headers: OrderedMap.Builder<string, string>) =
    let workflowDetails = sim.WorkflowDetails
    let trigger = sim.TriggerResult
    let correlationId = trigger.trackingId

    headers
        .TryAdd("x-ms-workflow-run-id", workflowDetails.run.name)
        .TryAdd("x-ms-correlation-id", correlationId)
        .TryAdd("x-ms-client-tracking-id", trigger.clientTrackingId)
        .MaybeTryAdd("x-ms-trigger-history-name", trigger.originHistoryName)
        .TryAdd("x-ms-workflow-system-id", $"/scaleunits/prod-00{workflowDetails.id}")
        .TryAdd("x-ms-workflow-id", workflowDetails.id.Split('/') |> Array.last)
        .TryAdd("x-ms-workflow-version", workflowDetails.version)
        .TryAdd("x-ms-workflow-name", workflowDetails.name)
        .TryAdd("x-ms-tracking-id", correlationId)
        .TryAdd("x-ms-request-id", $":{correlationId}")

let addWorkflowResponseHeaders (sim: SimulatorContext) (headers: OrderedMap<string, string>) =
    OrderedMap.Builder().AddRange(headers)
    |> addWorkflowResponseHeadersToBuilder sim
    |> _.Build()

type ActionGraphContainer =
    { actions: ActionGraph }

    static member Default = { actions = OrderedMap.empty }

let actionGraphContainerOfJson resolveAction json =
    { actions = JsonTree.getKey "actions" json |> actionGraphOfJson resolveAction }

type SwitchCase =
    { actions: ActionGraph; case: JsonTree }

let switchCaseOfJson resolveAction json =
    { actions = JsonTree.getKey "actions" json |> actionGraphOfJson resolveAction
      case = JsonTree.getKey "case" json }

type UntilLimit = { count: int64; timeout: string }

let untilLimitOfJson json =
    { count = JsonTree.getKey "count" json |> Conversions.ensureInteger
      timeout = JsonTree.getKey "timeout" json |> Conversions.ensureString }

type TerminateInputs =
    { runStatus: string
      runError: TerminateRunError option }

let terminateInputsOfJson json =
    { runStatus = JsonTree.getKey "runStatus" json |> Conversions.ensureString
      runError = JsonTree.tryGetKey "runError" json |> Option.map terminateRunErrorOfJson }

let jsonOfTerminateInputs (inputs: TerminateInputs) =
    OrderedMap
        .Builder()
        .Add("runStatus", JsonTree.String inputs.runStatus)
        .MaybeAdd("runError", inputs.runError |> Option.map jsonOfTerminateRunError)
        .Build()
    |> JsonTree.Object

type HttpInputs =
    { method: string
      uri: string
      headers: OrderedMap<string, string> option
      queries: OrderedMap<string, string> option
      body: JsonTree
      cookie: string option
      authentication: JsonTree
      retryPolicy: JsonTree option }

    member inputs.ToJson() =
        OrderedMap
            .Builder()
            .Add("method", JsonTree.String inputs.method)
            .Add("uri", JsonTree.String inputs.uri)
            .MaybeAdd("headers", inputs.headers)
            .MaybeAdd("queries", inputs.queries)
            .MaybeAdd("body", inputs.body)
            .MaybeAdd("cookie", inputs.cookie)
            .MaybeAdd("authentication", inputs.authentication)
            .MaybeAdd("retryPolicy", inputs.retryPolicy)
            .Build()
        |> JsonTree.Object

    override this.ToString() = this.ToJson().ToString()

let inline jsonOfHttpInputs (inputs: HttpInputs) = inputs.ToJson()

let httpInputsOfJson json =
    { method = JsonTree.getKey "method" json |> Conversions.ensureString
      uri = JsonTree.getKey "uri" json |> Conversions.ensureString
      headers = JsonTree.tryGetKey "headers" json |> Option.map Conversions.stringsMapOfJson
      queries = JsonTree.tryGetKey "queries" json |> Option.map Conversions.stringsMapOfJson
      body = JsonTree.getKeyOrNull "body" json
      cookie = JsonTree.tryGetKey "cookie" json |> Option.map Conversions.ensureString
      authentication = JsonTree.getKeyOrNull "authentication" json
      retryPolicy = JsonTree.tryGetKey "retryPolicy" json }

let defaultExpression () : Expression = Conversions.emptyObject

let getVarType (value: JsonTree) : VariableType =
    match JsonTree.getType value with
    | JsonType.Null
    | JsonType.Object -> VariableType.Object
    | JsonType.Array -> VariableType.Array
    | JsonType.String -> VariableType.String
    | JsonType.Integer -> VariableType.Integer
    | JsonType.Float
    | JsonType.Decimal -> VariableType.Float
    | JsonType.Boolean -> VariableType.Boolean

let coerce (typ: VariableType) (value: JsonTree) : JsonTree =
    match typ, value with
    | VariableType.String, JsonTree.String _ -> value
    | VariableType.String, Null -> JsonTree.String ""
    | VariableType.Integer, JsonTree.Integer _ -> value
    | VariableType.Integer, JsonTree.Null -> JsonTree.Integer 0
    | VariableType.Float, (JsonTree.Float _ | JsonTree.Decimal _) -> value
    | VariableType.Float, JsonTree.Integer i -> JsonTree.Float(float i)
    | VariableType.Float, JsonTree.Null -> JsonTree.Float 0.0
    | VariableType.Boolean, JsonTree.Boolean _ -> value
    | VariableType.Boolean, JsonTree.Null -> JsonTree.Boolean false
    | VariableType.Object, JsonTree.Object _ -> value
    | VariableType.Object, JsonTree.Null -> Null
    | VariableType.Array, JsonTree.Array _ -> value
    | VariableType.Array, JsonTree.Null -> Conversions.emptyArray
    | typ, value -> failwithf "Expected %O, got %O" typ (JsonTree.getType value)

let getVarTypechecked (context: SimulatorContext) var typs =
    match context.GetVariable var with
    | None -> failwithf "Variable '%s' does not exist" var
    | Some originalValue ->
        let variableType = getVarType originalValue

        if not (Seq.contains variableType typs) then
            failwithf "Variable is of type %O, expected one of %O" variableType typs

        originalValue

let codeAndErrorFromScopeResult result =
    match result with
    | Succeeded -> NotSpecified, None
    | _ ->
        ActionFailed,
        Some
            { ActionError.code = ActionFailed
              message = "An action failed. No dependent actions succeeded." }
