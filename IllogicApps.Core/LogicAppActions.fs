namespace IllogicApps.Core.LogicAppActions

open System.Collections.Generic
open System.Text.Json.Nodes

open IllogicApps.Core
open IllogicApps.Core.LogicAppSpec
open IllogicApps.Core.LogicAppActionSupport
open IllogicApps.Core.LogicAppBaseAction
open System.Text.Json

// Triggers

type Request() =
    inherit Action()

    member val Kind = "" with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Trigger: %s" this.Kind

        { status = Succeeded
          inputs = None
          outputs = Some context.TriggerOutput }

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
          outputs = Some(makeObject [ "expression", JsonValue.Create(conditionResult) ]) }

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
          outputs = Some(makeObject [ "expression", value ]) }

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

        let parsedTimeout = System.TimeSpan.Parse(this.Limit.timeout)
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

        this.Inputs.variables
        |> List.map (fun variable ->
            let value =
                variable.Value
                |> Option.defaultValue (defaultForType variable.VariableType)
                |> context.EvaluateLanguage
                |> coerce variable.VariableType in

            (variable.Name, value))
        |> List.iter (fun (name, value) ->
            printfn "InitializeVariable: %s = %O" name value

            if context.Variables.ContainsKey(name) then
                failwithf "Variable '%s' already exists" name

            context.Variables.[name] <- value)

        { status = Succeeded
          inputs =
            Some(
                // TODO probably not accurate
                makeObject [ "variables", JsonValue.Create(this.Inputs.variables) ]
            )
          outputs = None }

type SetVariable() =
    inherit Action()

    let typecheck (originalValue: JsonNode) (newValue: JsonNode) =
        let reverseType =
            function
            | JsonValueKind.Undefined -> failwith "Undefined value"
            | JsonValueKind.Object -> Object
            | JsonValueKind.Array -> Array
            | JsonValueKind.String -> String
            | JsonValueKind.Number -> Float
            | JsonValueKind.True
            | JsonValueKind.False -> Boolean
            | JsonValueKind.Null -> Object
            | _ -> failwith "Unknown enum value" in

        let originalType = reverseType <| originalValue.GetValueKind()
        let newType = reverseType <| newValue.GetValueKind()

        if originalType <> newType then
            failwithf "Variable is of type %A, cannot set to %A" originalType newType

    member val Inputs = new SetVariableSingle() with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "SetVariable: %s = %O" this.Inputs.Name this.Inputs.Value

        if not (context.Variables.ContainsKey(this.Inputs.Name)) then
            failwithf "Variable '%s' does not exist" this.Inputs.Name

        typecheck context.Variables.[this.Inputs.Name] this.Inputs.Value

        let value = this.Inputs.Value |> context.EvaluateLanguage

        context.Variables.[this.Inputs.Name] <- value

        let inputs =
            makeObject [ "name", JsonValue.Create(this.Inputs.Name); "value", value.DeepClone() ]

        { status = Succeeded
          inputs = Some(inputs)
          outputs = Some(makeObject [ "body", inputs.DeepClone() ]) }

type AppendToStringVariable() =
    inherit Action()

    member val Inputs = new SetVariableSingle() with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "%s: %s = %O" this.ActionType this.Inputs.Name this.Inputs.Value

        let originalValue = getVarTypechecked context this.Inputs.Name this.ExpectedType
        let addend = this.Inputs.Value |> context.EvaluateLanguage
        let newValue = this.Add originalValue addend
        context.Variables.[this.Inputs.Name] <- newValue

        { status = Succeeded
          inputs =
            Some(
                makeObject
                    [ ("name", JsonValue.Create(this.Inputs.Name))
                      ("value", this.Inputs.Value.DeepClone()) ]
            )
          // TODO check this for accuracy
          outputs =
            Some(
                makeObject
                    [ ("name", JsonValue.Create(this.Inputs.Name))
                      ("value", newValue.DeepClone()) ]
            ) }

    abstract member ExpectedType: JsonValueKind with get
    override this.ExpectedType = JsonValueKind.String

    abstract member Add: JsonNode -> JsonNode -> JsonNode

    override this.Add a b =
        JsonValue.Create(a.ToString() + b.ToString())

type AppendToArrayVariable() =
    inherit AppendToStringVariable()

    override _.ExpectedType = JsonValueKind.Array

    override this.Add a b =
        let array = a.DeepClone().AsArray()
        b.GetValueKind() |> function
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

        let originalValue = getVarTypechecked context this.Inputs.Name JsonValueKind.Number
        let increment = this.Inputs.Value |> context.EvaluateLanguage
        let value = this.Add (originalValue.GetValue<int>()) (increment.GetValue<int>())
        context.Variables.[this.Inputs.Name] <- JsonValue.Create(value)

        { status = Succeeded
          inputs =
            Some(
                makeObject
                    [ "name", JsonValue.Create(this.Inputs.Name)
                      "value", originalValue.DeepClone() ]
            )
          outputs = Some(makeObject [ "name", JsonValue.Create(this.Inputs.Name); "value", JsonValue.Create(value) ]) }

    abstract member Add: int -> int -> int
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
          inputs = Some(makeObject [ "content", input; "schema", this.Inputs.schema ])
          outputs = Some(makeObject [ "body", result ]) }

type Query() =
    inherit Action()

    member val Inputs: QueryInputs =
        { from = JsonValue.Create(null)
          where = JsonValue.Create(null) } with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Query: %O in %O" this.Inputs.where this.Inputs.from

        let from = context.EvaluateLanguage this.Inputs.from
        let where = context.EvaluateLanguage this.Inputs.where

        let arrayVals = from.AsArray()
        use loopContext = context.PushLoopContext arrayVals

        let rec filterValsRev acc =
            let current = loopContext.Current
            let condition = where |> context.EvaluateLanguage
            let next = if condition.GetValue<bool>() then current :: acc else acc
            if loopContext.Advance() then filterValsRev next else next

        let result = filterValsRev [] |> List.rev in

        printfn "Query Result: %O" result

        { status = Succeeded
          inputs = Some(makeObject [ "from", from.DeepClone() ])
          outputs = Some(makeObject [ "body", new JsonArray(result |> List.toArray) ]) }

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
                |> addKeyValuePair "headers" (processedHeaders |> Option.map (kvToJsonValues >> makeObject))
            )
            |> context.EvaluateLanguage

        context.ExternalServiceRequest
        <| ExternalServiceTypes.HttpResponse(
            new ExternalServiceTypes.HttpRequestReply(
                StatusCode = processedStatusCode.GetValue<int>(),
                Headers = (processedHeaders |> Option.map Map.ofSeq |> Option.defaultValue Map.empty),
                Body =
                    (processedBody
                     |> Option.map _.DeepClone()
                     |> Option.defaultValue (JsonValue.Create(null)))
            )
        )

        { status = Succeeded
          inputs = Some(inputsObject)
          outputs = None }

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

        let result = ref <| new ExternalServiceTypes.HttpRequestReply() in

        context.ExternalServiceRequest
        <| ExternalServiceTypes.HttpRequest(
            new ExternalServiceTypes.HttpRequest(
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
          outputs = Some(JsonValue.Create(result).Deserialize<JsonObject>()) }

// Workflow actions

type Workflow() =
    inherit Action()

    member val Inputs: WorkflowInputs = new WorkflowInputs() with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Workflow: %s" (JsonSerializer.Serialize(this))

        printfn "Unimplemented" // TODO

        { status = Succeeded
          inputs = Some(JsonValue.Create(this.Inputs).Deserialize<JsonObject>())
          outputs = None }
