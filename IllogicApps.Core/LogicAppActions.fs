namespace IllogicApps.Core.LogicAppActions

open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Nodes
open System.Xml

open IllogicApps.Core
open IllogicApps.Core.LogicAppSpec
open IllogicApps.Core.LogicAppActionSupport
open CompletedStepTypes
open ExternalServiceTypes
open JsonUtil

// Triggers

type Request() =
    inherit Action()

    member val Kind = "" with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Trigger: %s" this.Kind

        // TODO: Does this ever get called?

        let triggerResult = context.TriggerResult

        { status = triggerResult.action.status
          inputs = Option.ofObj (systemTextJsonOfIllogicJson triggerResult.action.inputs)
          outputs = Option.ofObj (systemTextJsonOfIllogicJson triggerResult.action.outputs) }

// Actions

// Control actions

type Scope() =
    inherit Action()

    member val Actions: ActionGraph = Map.empty with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Scope Begin"
        let result = context.ExecuteGraph this.Actions in
        printfn "Scope End"

        { status = result
          inputs = None
          outputs = None }

    override this.GetChildren() : (string * IGraphExecutable) list =
        this.Actions
        |> Seq.map (fun kv -> kv.Key, (kv.Value: IGraphExecutable))
        |> Seq.toList

type If() =
    inherit Scope()

    member val Expression = defaultExpression () with get, set
    member val Else: ActionGraphContainer = new ActionGraphContainer() with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "If Begin"
        let conditionResult = context.EvaluateCondition this.Expression in // TODO
        printfn "If Condition: %b" conditionResult

        let (actions, otherActions) =
            if conditionResult then
                this.Actions, this.Else.Actions
            else
                this.Else.Actions, this.Actions in

        otherActions |> fromKvps |> context.ForceSkipAll

        let result = context.ExecuteGraph actions in
        printfn "If End"

        { status = result
          inputs = None
          outputs = Some(jsonOf [ "expression", jsonOf conditionResult ]) }

    override this.GetChildren() : (string * IGraphExecutable) list =
        Seq.append this.Actions this.Else.Actions
        |> Seq.map (fun kv -> kv.Key, (kv.Value: IGraphExecutable))
        |> Seq.toList


type Switch() =
    inherit Action()

    member val Expression: JsonNode = JsonValue.Create(null) with get, set
    member val Default = new ActionGraphContainer() with get, set
    member val Cases: Map<string, SwitchCase> = Map.empty with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Switch Begin"
        let value = context.EvaluateLanguage this.Expression in
        printfn "Switch Value: %O" value

        let actions =
            this.Cases.Values
            |> Seq.tryFind (fun case -> case.Case = value)
            |> Option.map _.Actions
            |> Option.defaultValue this.Default.Actions in

        this.Cases.Values
        |> Seq.map _.Actions
        |> Seq.append [ this.Default.Actions ]
        |> Seq.filter (fun selectedActions -> selectedActions <> actions)
        |> Seq.collect fromKvps
        |> context.ForceSkipAll

        let result = context.ExecuteGraph actions in

        printfn "Switch End"

        { status = result
          inputs = None
          outputs = Some(jsonOf [ "expression", value ]) }

    override this.GetChildren() =
        this.Cases.Values
        |> Seq.fold (fun acc case -> Seq.append acc case.Actions) this.Default.Actions
        |> Seq.map (fun kv -> kv.Key, (kv.Value: IGraphExecutable))
        |> Seq.toList

type Until() =
    inherit Scope()

    member val Expression: JsonValue = JsonValue.Create("") with get, set
    member val Limit = { count = 0; timeout = "" } with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Until Begin"

        let parsedTimeout = XmlConvert.ToTimeSpan(this.Limit.timeout)
        let timeout = System.DateTime.Now.Add(parsedTimeout)

        let rec attempt num =
            printfn "Until Attempt %d Begin" num
            let result = context.ExecuteGraph this.Actions in
            printfn "Until Attempt %d End" num

            match result with
            | Succeeded ->
                if System.DateTime.Now >= timeout then
                    printfn "Until Timeout"
                    TimedOut
                else if num >= this.Limit.count then
                    printfn "Until Limit"
                    result
                else
                    let condition = context.EvaluateLanguage(this.Expression).AsValue().GetValue<bool>() in
                    printfn "Until Condition: %b" condition
                    if condition then attempt (num + 1) else result
            | Skipped -> failwith "Overall result is Skipped"
            | Cancelled
            | Failed
            | TimedOut -> result

        let result = attempt 1
        printfn "Until End"

        { status = result
          inputs = None // TODO
          outputs = None // TODO
        }

type Terminate() =
    inherit Action()

    member val Inputs = { runError = None; runStatus = "" } with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Terminate: %s (%O)" this.Inputs.runStatus this.Inputs.runError

        context.StopExecuting
        <| match this.Inputs.runStatus with
           | "Failed" -> Failed
           | "Succeeded" -> Succeeded
           | "Cancelled" -> Skipped
           | _ -> Failed

        { status = Succeeded
          inputs = Some(JsonValue.Create(this.Inputs)) // TODO check this
          outputs = None }



// Variable actions

type InitializeVariable() =
    inherit Action()

    member val Inputs = { variables = ([]: InitializeVariableSingle list) } with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "InitializeVariable"

        let processedVars =
            this.Inputs.variables
            |> List.map (fun variable ->
                let value =
                    variable.Value
                    |> Option.defaultValue (defaultForType variable.VariableType)
                    |> context.EvaluateLanguage
                    |> coerce variable.VariableType in

                (variable.Name, variable.VariableType, value))

        processedVars
        |> List.iter (fun (name, type_, value) ->
            printfn "InitializeVariable: %s = %O" name value

            if context.Variables.ContainsKey(name) then
                failwithf "Variable '%s' already exists" name

            context.Variables.[name] <- value)

        let processedVarsArray =
            processedVars
            |> List.map (fun (name, type_, value) ->
                jsonOf
                    [ "name", jsonOf name
                      "type", jsonOf (type_.ToString())
                      "value", value.DeepClone() ])
            |> List.toArray
            |> jsonOf

        { status = Succeeded
          inputs = Some(jsonOf [ "variables", processedVarsArray ])
          outputs = None }

type SetVariable() =
    inherit Action()

    let typecheck (originalValue: JsonNode) (newValue: JsonNode) =
        if getVarType originalValue <> getVarType newValue then
            failwithf "Variable is of type %A, cannot set to %A" (getVarType originalValue) (getVarType newValue)

    member val Inputs = new SetVariableSingle() with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "SetVariable: %s = %O" this.Inputs.Name this.Inputs.Value

        if not (context.Variables.ContainsKey(this.Inputs.Name)) then
            failwithf "Variable '%s' does not exist" this.Inputs.Name

        typecheck context.Variables.[this.Inputs.Name] this.Inputs.Value

        let value = this.Inputs.Value |> context.EvaluateLanguage

        context.Variables.[this.Inputs.Name] <- value

        let inputs = jsonOf [ "name", jsonOf this.Inputs.Name; "value", value.DeepClone() ]

        { status = Succeeded
          inputs = Some(inputs)
          outputs = Some(jsonOf [ "body", inputs.DeepClone() ]) }

type AppendToStringVariable() =
    inherit Action()

    member val Inputs = new SetVariableSingle() with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "%s: %s = %O" this.ActionType this.Inputs.Name this.Inputs.Value

        let originalValue = getVarTypechecked context this.Inputs.Name [ this.ExpectedType ]
        let addend = this.Inputs.Value |> context.EvaluateLanguage
        let newValue = this.Add originalValue addend
        context.Variables.[this.Inputs.Name] <- newValue

        { status = Succeeded
          inputs = Some(jsonOf [ "name", jsonOf this.Inputs.Name; "value", this.Inputs.Value.DeepClone() ])
          outputs = None }

    abstract member ExpectedType: VariableType with get
    override this.ExpectedType = VariableType.String

    abstract member Add: JsonNode -> JsonNode -> JsonNode

    override this.Add a b =
        JsonValue.Create(a.ToString() + b.ToString())

type AppendToArrayVariable() =
    inherit AppendToStringVariable()

    override _.ExpectedType = VariableType.Array

    override this.Add a b =
        let array = a.DeepClone().AsArray()

        b.GetValueKind()
        |> function
            | JsonValueKind.Number
            | JsonValueKind.String
            | JsonValueKind.True
            | JsonValueKind.False
            | JsonValueKind.Object -> array.Add(b.DeepClone())
            | _ -> failwithf "%s: Unsupported value type %A" (this.ActionType) (b.GetValueKind())

        array

type IncrementVariable() =
    inherit SetVariable()

    override this.Execute(context: SimulatorContext) =
        printfn "%s: %s" this.ActionType this.Inputs.Name

        let originalValue =
            getVarTypechecked context this.Inputs.Name [ VariableType.Integer; VariableType.Float ]

        let increment = this.Inputs.Value |> context.EvaluateLanguage
        let value = this.Add (originalValue.GetValue<int64>()) (increment.GetValue<int64>())
        context.Variables.[this.Inputs.Name] <- JsonValue.Create(value)

        { status = Succeeded
          inputs = Some(jsonOf [ "body", jsonOf [ "name", jsonOf this.Inputs.Name; "value", increment.DeepClone() ] ])
          outputs = Some(jsonOf [ "name", jsonOf this.Inputs.Name; "value", jsonOf value ]) }

    abstract member Add: int64 -> int64 -> int64
    override this.Add a b = a + b

type DecrementVariable() =
    inherit IncrementVariable()

    override this.Add a b = a - b

// Data Operations actions

type Compose() =
    inherit Action()

    member val Inputs: JsonNode = JsonValue.Create(null) with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Compose: %O" this.Inputs
        let result = context.EvaluateLanguage this.Inputs
        printfn "Compose Result: %O" result

        { status = Succeeded
          inputs = Some(result)
          outputs = Some(result) }

type ParseJson() =
    inherit Action()

    member val Inputs: ParseJsonInputs =
        { content = JsonValue.Create(null)
          schema = JsonValue.Create(null) } with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "ParseJson: %O" this.Inputs
        let input = context.EvaluateLanguage this.Inputs.content
        let result = input.ToString() |> JsonNode.Parse
        // TODO: schema
        // TODO: can we do freaky variable stuff in the schema?
        printfn "ParseJson Result: %O" result

        { status = Succeeded
          inputs = Some(jsonOf [ "content", input; "schema", this.Inputs.schema ])
          outputs = Some(jsonOf [ "body", result ]) }

type Query() =
    inherit Action()

    member val Inputs: QueryInputs =
        { from = JsonValue.Create(null)
          where = JsonValue.Create(null) } with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Query: %O in %O" this.Inputs.where this.Inputs.from

        let from = context.EvaluateLanguage this.Inputs.from

        let arrayVals = from.AsArray()
        use loopContext = context.PushArrayOperationContext arrayVals

        let rec filterValsRev acc =
            let current = loopContext.Current
            let condition = this.Inputs.where |> context.EvaluateLanguage

            let next = if condition.GetValue<bool>() then current :: acc else acc

            if loopContext.Advance() then filterValsRev next else next

        let result = filterValsRev [] |> List.rev in

        printfn "Query Result: %O" result

        { status = Succeeded
          inputs = Some(from.DeepClone())
          outputs = Some(jsonOf [ "body", jsonOf (result |> List.map systemTextJsonOfIllogicJson) ]) }

// Inline Code actions

type JavaScriptCode() =
    inherit Action()

    // TODO: correct inputs
    member val Inputs: JsonNode = JsonValue.Create(null) with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "NOT IMPLEMENTED JavaScriptCode: %O" this.Inputs

        { status = Failed
          inputs = None
          outputs = None }

// Request actions

type Response() =
    inherit Action()

    member val Inputs =
        { body = None
          headers = None
          statusCode = JsonValue.Create(0) } with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Response: %O" this.Inputs

        let inline addKeyValuePair (key: string) (value: JsonNode option) (seq: seq<KeyValuePair<string, JsonNode>>) =
            value
            |> Option.map (fun v -> new KeyValuePair<string, JsonNode>(key, v))
            |> Option.toList
            |> Seq.append seq

        let inline kvToJsonValues seq =
            Seq.map (fun (k, v) -> k, (JsonValue.Create(box v): JsonNode)) seq

        let processedBody = this.Inputs.body |> Option.map context.EvaluateLanguage

        let processedHeaders =
            this.Inputs.headers
            |> Option.map (Seq.map (fun kv -> kv.Key, (context.EvaluateLanguage kv.Value).ToString()))

        let processedStatusCode = context.EvaluateLanguage(this.Inputs.statusCode)

        let inputsObject =
            new JsonObject(
                [ new KeyValuePair<string, JsonNode>("statusCode", processedStatusCode) ]
                |> addKeyValuePair "body" processedBody
                |> addKeyValuePair "headers" (processedHeaders |> Option.map (kvToJsonValues >> jsonOf))
            )
            |> context.EvaluateLanguage

        context.ExternalServiceRequest
        <| HttpResponse(
            new HttpRequestReply(
                StatusCode = processedStatusCode.GetValue<int>(),
                Headers = (processedHeaders |> Option.map Map.ofSeq |> Option.defaultValue Map.empty),
                Body = (processedBody |> Option.map _.DeepClone() |> Option.defaultValue jsonNull)
            )
        )

        { status = Succeeded
          inputs = Some(inputsObject)
          outputs = Some(inputsObject) }

// HTTP actions

type Http() =
    inherit Action()

    member val Inputs: HttpInputs =
        { method = ""
          uri = ""
          headers = None
          body = None
          queries = None
          cookie = None
          authentication = None } with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "HTTP: %A" this.Inputs

        let result = ref <| new HttpRequestReply() in

        context.ExternalServiceRequest
        <| HttpRequest(
            new HttpRequest(
                Method = this.Inputs.method,
                Uri = this.Inputs.uri,
                Headers = (this.Inputs.headers |> Option.defaultValue Map.empty),
                Body = (this.Inputs.body |> Option.map _.ToString()),
                QueryParameters = (this.Inputs.queries |> Option.defaultValue Map.empty),
                Cookie = this.Inputs.cookie,
                Authentication = this.Inputs.authentication
            ),
            result
        )

        { status = Succeeded
          inputs = Some(JsonValue.Create(this.Inputs).Deserialize<JsonObject>())
          outputs = Some(JsonValue.Create(result.Value).Deserialize<JsonObject>()) }

// Workflow actions

type Workflow() =
    inherit Action()

    member val Inputs = new WorkflowRequest() with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Workflow: %s" (JsonSerializer.Serialize(this))

        let headers =
            this.Inputs.Headers
            |> Map.map (fun _ v -> context.EvaluateLanguage v |> _.GetValue<string>())

        let body = this.Inputs.Body |> context.EvaluateLanguage

        let result = ref <| new HttpRequestReply() in

        let inline jsonClone (o: 'a) : 'a =
            JsonSerializer.Deserialize<'a>(JsonSerializer.Serialize(o))

        let request =
            new WorkflowRequest(
                Host = (this.Inputs.Host |> jsonClone),
                Headers = headers,
                Body = body.DeepClone(),
                RetryPolicy = (this.Inputs.RetryPolicy |> jsonClone)
            )

        context.ExternalServiceRequest
        <| ExternalServiceTypes.Workflow((jsonClone request), result)

        { status = Succeeded
          inputs = Some(JsonValue.Create(request))
          outputs = Some(JsonValue.Create(result.Value)) }
