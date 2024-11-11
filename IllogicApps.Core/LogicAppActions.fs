namespace IllogicApps.Core.LogicAppActions

open System.Xml

open IllogicApps.Core
open IllogicApps.Core.LogicAppBaseAction
open IllogicApps.Core.LogicAppSpec
open IllogicApps.Core.LogicAppActionSupport
open CompletedStepTypes
open ExternalServiceTypes
open IllogicApps.Json

// Triggers

type Request(json) =
    inherit BaseAction(json)

    member val Kind = JsonTree.getKey "kind" json |> Conversions.ensureString with get

    override this.Execute(context: SimulatorContext) =
        printfn "Trigger: %s" this.Kind

        // TODO: Does this ever get called?

        let triggerResult = context.TriggerResult

        { status = triggerResult.action.status
          inputs = triggerResult.action.inputs
          outputs = triggerResult.action.outputs
          code = triggerResult.action.code
          error = triggerResult.action.error }

// Actions

// Control actions

type Scope(resolveAction, json) =
    inherit BaseAction(json)

    member val Actions = JsonTree.getKey "actions" json |> actionGraphOfJson resolveAction with get

    override this.Execute(context: SimulatorContext) =
        printfn "Scope Begin"
        let result = context.ExecuteGraph this.Actions in
        printfn "Scope End"

        let code, error = codeAndErrorFromScopeResult result

        { ActionResult.Default with
            status = result
            code = Some code
            error = error }

    override this.GetChildren() : (string * IGraphExecutable) list =
        this.Actions
        |> Seq.map (fun kv -> kv.Key, (kv.Value: IGraphExecutable))
        |> Seq.toList

type If(resolveAction, json) =
    inherit Scope(resolveAction, json)

    member val Expression = JsonTree.getKey "expression" json with get
    member val Else = JsonTree.getKey "else" json |> actionGraphContainerOfJson resolveAction with get

    override this.Execute(context: SimulatorContext) =
        printfn "If Begin"
        let conditionResult = context.EvaluateCondition this.Expression in // TODO
        printfn "If Condition: %b" conditionResult

        let actions, otherActions =
            if conditionResult then
                this.Actions, this.Else.actions
            else
                this.Else.actions, this.Actions in

        otherActions |> fromKvps |> context.ForceSkipAll

        let result = context.ExecuteGraph actions in
        printfn "If End"

        let code, error = codeAndErrorFromScopeResult result

        { status = result
          inputs = None
          outputs = Some(Conversions.createObject [ "expression", Boolean conditionResult ])
          code = Some code
          error = error }

    override this.GetChildren() : (string * IGraphExecutable) list =
        Seq.append this.Actions this.Else.actions
        |> Seq.map (fun kv -> kv.Key, (kv.Value: IGraphExecutable))
        |> Seq.toList

type Switch(resolveAction, json) =
    inherit BaseAction(json)

    member val Expression = JsonTree.getKey "expression" json with get
    member val Default = JsonTree.getKey "default" json |> actionGraphContainerOfJson resolveAction with get

    member val Cases =
        JsonTree.getKey "cases" json
        |> Conversions.ensureObject
        |> OrderedMap.mapValuesOnly (switchCaseOfJson resolveAction) with get

    override this.Execute(context: SimulatorContext) =
        printfn "Switch Begin"
        let value = context.EvaluateLanguage this.Expression in
        printfn "Switch Value: %O" value

        let actions =
            this.Cases.Values
            |> Seq.tryFind (fun case -> case.case = value)
            |> Option.map _.actions
            |> Option.defaultValue this.Default.actions in

        this.Cases.Values
        |> Seq.map _.actions
        |> Seq.append [ this.Default.actions ]
        |> Seq.filter (fun selectedActions -> selectedActions <> actions)
        |> Seq.collect fromKvps
        |> context.ForceSkipAll

        let result = context.ExecuteGraph actions in

        printfn "Switch End"

        let code, error = codeAndErrorFromScopeResult result

        { status = result
          inputs = None
          outputs = Some(Conversions.createObject [ "expression", value ])
          code = Some code
          error = error }

    override this.GetChildren() =
        this.Cases.Values
        |> Seq.fold (fun acc case -> Seq.append acc case.actions) this.Default.actions
        |> Seq.map (fun kv -> kv.Key, (kv.Value: IGraphExecutable))
        |> Seq.toList

type Until(resolveAction, json) =
    inherit Scope(resolveAction, json)

    member val Expression = JsonTree.getKey "expression" json with get
    member val Limit = JsonTree.getKey "limit" json |> untilLimitOfJson with get

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
                    let condition =
                        context.EvaluateLanguage(this.Expression) |> Conversions.ensureBoolean in

                    printfn "Until Condition: %b" condition
                    if condition then attempt (num + 1L) else result
            | Skipped -> failwith "Overall result is Skipped"
            | Cancelled
            | Failed
            | TimedOut -> result

        let result = attempt 1L
        printfn "Until End"

        let code, error = codeAndErrorFromScopeResult result

        { status = result
          inputs = None // TODO
          outputs = None // TODO
          code = Some code
          error = error }

type Terminate(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> terminateInputsOfJson with get

    override this.Execute(context: SimulatorContext) =
        printfn "Terminate: %s (%O)" this.Inputs.runStatus this.Inputs.runError

        context.StopExecuting
        <| match this.Inputs.runStatus with
           | "Failed" -> Failed
           | "Succeeded" -> Succeeded
           | "Cancelled" -> Skipped
           | _ -> Failed

        { ActionResult.Default with
            inputs = Some(jsonOfTerminateInputs this.Inputs)
            code = Some NotSpecified }

// Variable actions

type InitializeVariable(json) =
    inherit BaseAction(json)

    member val Inputs =
        JsonTree.getKey "inputs" json
        |> variablesInputsOfJson initializeVariableSingleOfJson with get

    override this.Execute(context: SimulatorContext) =
        printfn "InitializeVariable"

        let processedVars =
            this.Inputs.variables
            |> List.map (fun variable ->
                let value = variable.value |> context.EvaluateLanguage |> coerce variable.type_ in

                (variable.name, variable.type_, value))

        processedVars
        |> List.iter (fun (name, type_, value) ->
            printfn "InitializeVariable: %s = %O" name value

            if context.Variables.ContainsKey(name) then
                failwithf "Variable '%s' already exists" name

            context.Variables.[name] <- value)

        let processedVarsArray =
            processedVars
            |> List.map (fun (name, type_, value) ->
                Conversions.createObject [ "name", String name; "type", String(type_.ToString()); "value", value ])
            |> Conversions.createArray

        { ActionResult.Default with
            inputs = Some(Conversions.createObject [ "variables", processedVarsArray ])
            code = Some NotSpecified }

type SetVariable(json) =
    inherit BaseAction(json)

    let typecheck (originalValue: JsonTree) (newValue: JsonTree) =
        if getVarType originalValue <> getVarType newValue then
            failwithf "Variable is of type %A, cannot set to %A" (getVarType originalValue) (getVarType newValue)

    member val Inputs = JsonTree.getKey "inputs" json |> setVariableSingleOfJson with get

    override this.Execute(context: SimulatorContext) =
        printfn "SetVariable: %s = %O" this.Inputs.name this.Inputs.value

        if not (context.Variables.ContainsKey(this.Inputs.name)) then
            failwithf "Variable '%s' does not exist" this.Inputs.name

        let value = this.Inputs.value |> context.EvaluateLanguage

        typecheck context.Variables.[this.Inputs.name] value

        context.Variables.[this.Inputs.name] <- value

        let inputs =
            Conversions.createObject [ "name", String this.Inputs.name; "value", value ]

        { ActionResult.Default with
            inputs = Some(inputs)
            outputs = Some(Conversions.createObject [ "body", inputs ])
            code = Some NotSpecified }

type AppendToStringVariable(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> setVariableSingleOfJson with get

    override this.Execute(context: SimulatorContext) =
        printfn "%s: %s = %O" this.ActionType this.Inputs.name this.Inputs.value

        if not (context.Variables.ContainsKey(this.Inputs.name)) then
            failwithf "Variable '%s' does not exist" this.Inputs.name

        let originalValue = context.Variables[this.Inputs.name]
        let addend = this.Inputs.value |> context.EvaluateLanguage
        let newValue = this.Add context originalValue addend
        context.Variables.[this.Inputs.name] <- newValue

        { ActionResult.Default with
            inputs = Some(Conversions.createObject [ "name", String this.Inputs.name; "value", this.Inputs.value ])
            code = Some NotSpecified }

    abstract member Add: SimulatorContext -> JsonTree -> JsonTree -> JsonTree

    override this.Add context a b =
        String((Conversions.ensureString a) + (Conversions.ensureString b))

type AppendToArrayVariable(json) =
    inherit AppendToStringVariable(json)

    override this.Add context a b =
        if context.IsBugForBugAccurate then
            match b with
            | Array _ -> failwith "Cannot append an array to an array"
            | Null -> failwith "Cannot append null to an array"
            | _ -> ()

        Conversions.ensureArray a |> _.Add(b) |> Array

type IncrementVariable(json) =
    inherit SetVariable(json)

    override this.Execute(context: SimulatorContext) =
        printfn "%s: %s" this.ActionType this.Inputs.name

        let originalValue =
            getVarTypechecked context this.Inputs.name [ VariableType.Integer; VariableType.Float ]

        let increment = this.Inputs.value |> context.EvaluateLanguage

        let value =
            this.Add (originalValue |> Conversions.ensureInteger) (increment |> Conversions.ensureInteger)

        context.Variables.[this.Inputs.name] <- Integer value

        { ActionResult.Default with
            inputs =
                Some(
                    Conversions.createObject
                        [ "body", Conversions.createObject [ "name", String this.Inputs.name; "value", increment ] ]
                )
            outputs = Some(Conversions.createObject [ "name", String this.Inputs.name; "value", Integer value ])
            code = Some NotSpecified }

    abstract member Add: int64 -> int64 -> int64
    override this.Add a b = a + b

type DecrementVariable(json) =
    inherit IncrementVariable(json)

    override this.Add a b = a - b

// Data Operations actions

type Compose(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json with get

    override this.Execute(context: SimulatorContext) =
        printfn "Compose: %O" this.Inputs
        let result = context.EvaluateLanguage this.Inputs
        printfn "Compose Result: %O" result

        { ActionResult.Default with
            inputs = Some(result)
            outputs = Some(result)
            code = Some OK }

type ParseJson(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> parseJsonInputsOfJson with get

    override this.Execute(context: SimulatorContext) =
        printfn "ParseJson: %O" this.Inputs
        let input = context.EvaluateLanguage this.Inputs.content

        let result =
            match input with
            | String input -> input |> Parser.parse
            | json -> json
        // TODO: schema
        // TODO: can we do freaky variable stuff in the schema?
        printfn "ParseJson Result: %O" result

        { ActionResult.Default with
            inputs = Some(Conversions.createObject [ "content", input; "schema", this.Inputs.schema ])
            outputs = Some(Conversions.createObject [ "body", result ])
            code = Some OK }

type Query(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> queryInputsOfJson with get

    override this.Execute(context: SimulatorContext) =
        printfn "Query: %O in %O" this.Inputs.where this.Inputs.from

        let from = context.EvaluateLanguage this.Inputs.from

        let arrayVals = from |> Conversions.ensureArray
        use loopContext = context.PushArrayOperationContext arrayVals

        let rec filterValsRev acc =
            let current = loopContext.Current

            let condition =
                this.Inputs.where |> context.EvaluateLanguage |> Conversions.ensureBoolean

            let next = if condition then current :: acc else acc

            if loopContext.Advance() then filterValsRev next else next

        let result = filterValsRev [] |> List.rev in

        printfn "Query Result: %O" result

        { ActionResult.Default with
            inputs = Some(from)
            outputs = Some(Conversions.createObject [ "body", Conversions.createArray result ]) }

// Inline Code actions

type JavaScriptCode(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> javaScriptCodeInputsOfJson with get

    override this.Execute(context: SimulatorContext) =
        printfn "JavaScriptCode: %O" this.Inputs

        let result = ref (Error "Not executed")

        let processedCode =
            this.Inputs.code
            |> String
            |> context.EvaluateLanguage
            |> Conversions.ensureString

        context.ExternalServiceRequest
        <| ScriptExecution(
            { source = Inline processedCode
              language = JavaScript
              actions = context.AllActionResults
              workflow = context.WorkflowDetails
              trigger = context.TriggerResult },
            result
        )

        match result.Value with
        | Error message -> failwith message
        | Ok result ->
            let procesedInputs =
                { this.Inputs with
                    code = processedCode }

            { ActionResult.Default with
                inputs = Some(jsonOfJavaScriptCodeInputs procesedInputs)
                outputs = Some result
                code = Some OK }

// Key Vault actions

type ServiceProvider(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> serviceProviderInputsOfJson with get

    override this.Execute(context: SimulatorContext) =
        printfn "ServiceProvider: %O" this.Inputs

        let result = ref HttpRequestReply.Default

        let processedInputs =
            { this.Inputs with
                parameters = context.EvaluateLanguage(this.Inputs.parameters) }

        context.ExternalServiceRequest
        <| ExternalServiceTypes.ServiceProvider(
            { parameters = processedInputs.parameters |> context.EvaluateLanguage
              serviceProviderConfiguration = processedInputs.serviceProviderConfiguration },
            result
        )

        { ActionResult.Default with
            inputs = Some(jsonOfServiceProviderInputs processedInputs)
            outputs = Some(jsonOfHttpRequestReply result.Value) }

// Request actions

type Response(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> httpResponseInputsOfJson with get

    override this.Execute(context: SimulatorContext) =
        printfn "Response: %O" this.Inputs

        let inline stringPairSeqToObject seq =
            Seq.map (fun (k, v) -> k, String v) seq |> Conversions.createObject

        let processedBody = this.Inputs.body |> context.EvaluateLanguage

        let processedHeaders =
            this.Inputs.headers
            |> Option.map (
                Seq.map (fun (KeyValue(k, v)) ->
                    k, (v |> String |> context.EvaluateLanguage |> Conversions.rawStringOfJson))
            )

        let processedStatusCode = context.EvaluateLanguage(this.Inputs.statusCode)

        context.ExternalServiceRequest
        <| HttpResponse(
            { HttpRequestReply.statusCode = processedStatusCode |> Conversions.ensureInteger |> int
              headers = Option.map OrderedMap.ofSeq processedHeaders
              body = Conversions.optionOfJson processedBody }
        )

        let inputsObject =
            OrderedMap
                .Builder()
                .Add("statusCode", processedStatusCode)
                .MaybeAdd("body", processedBody)
                .MaybeAdd("headers", Option.map stringPairSeqToObject processedHeaders)
                .Build()
            |> Object

        { ActionResult.Default with
            inputs = Some(inputsObject)
            outputs = Some(inputsObject) }

// HTTP actions

type Http(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> httpInputsOfJson with get

    override this.Execute(context: SimulatorContext) =
        printfn "HTTP: %A" this.Inputs

        let result = ref HttpRequestReply.Default in

        context.ExternalServiceRequest
        <| HttpRequest(
            { method = this.Inputs.method
              uri = this.Inputs.uri
              headers = (this.Inputs.headers |> Option.defaultValue OrderedMap.empty)
              body =
                (this.Inputs.body
                 |> Conversions.optionOfJson
                 |> Option.map Conversions.rawStringOfJson)
              queryParameters = (this.Inputs.queries |> Option.defaultValue OrderedMap.empty)
              cookie = this.Inputs.cookie
              authentication = this.Inputs.authentication },
            result
        )

        { ActionResult.Default with
            inputs = Some(jsonOfHttpInputs this.Inputs)
            outputs = Some(jsonOfHttpRequestReply result.Value) }

// Workflow actions

type Workflow(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> workflowRequestOfJson with get

    member val OperationOptions =
        JsonTree.tryGetKey "operationOptions" json
        |> Option.map Conversions.ensureString with get

    override this.Execute(context: SimulatorContext) =
        printfn "Workflow: %s" (jsonOfWorkflowRequest this.Inputs |> Conversions.stringOfJson)

        let headers =
            this.Inputs.headers
            |> OrderedMap.map (fun k v ->
                context.EvaluateLanguage(String k) |> Conversions.ensureString,
                context.EvaluateLanguage(String v) |> Conversions.ensureString)

        let body = this.Inputs.body |> context.EvaluateLanguage

        let asyncSupported =
            not (this.OperationOptions |> Option.exists ((=) "DisableAsyncPattern"))

        let result =
            ref
                { HttpRequestReply.Default with
                    statusCode = System.Int32.MinValue }

        let request =
            { workflowId = this.Inputs.workflowId
              headers = headers
              body = body
              asyncSupported = asyncSupported
              retryPolicy = this.Inputs.retryPolicy }

        context.ExternalServiceRequest <| ExternalServiceTypes.Workflow(request, result)

        if result.Value.statusCode = System.Int32.MinValue then
            { ActionResult.Default with
                status = Failed
                inputs = Some(jsonOfWorkflowRequest this.Inputs)
                code = Some BadGateway
                error =
                    Some
                        { code = NoResponse
                          message = "The server did not receive a response from an upstream server." } }
        else
            { ActionResult.Default with
                inputs = Some(jsonOfWorkflowRequest request)
                outputs = Some(jsonOfHttpRequestReply result.Value)
                code = Some Accepted }
