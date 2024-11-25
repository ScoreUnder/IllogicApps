namespace IllogicApps.Core.LogicAppActions

open System.Net
open System.Xml

open IllogicApps.Core
open IllogicApps.Core.LogicAppSpec
open IllogicApps.Core.LogicAppActionSupport
open CompletedStepTypes
open ExternalServiceTypes
open IllogicApps.Core.HttpModel.HttpParsing
open IllogicApps.Core.HttpModel.HttpWriting
open IllogicApps.Core.Support
open IllogicApps.Json

// Triggers

type Request(json) =
    inherit BaseTrigger(json)

    let kind = JsonTree.getKey "kind" json |> Conversions.ensureString

    let inputs =
        JsonTree.tryGetKeyCaseInsensitive "inputs" json
        |> Option.map httpRequestTriggerInputsOfJson

    do
        match inputs with
        | Some { method = None; relativePath = Some _ } ->
            failwith $"{nameof Request} trigger: Method is required when relativePath is specified"
        | _ -> ()

        match kind with
        | "Http" -> ()
        | _ -> printfn "WARN: %s trigger: Kind '%s' is not supported" (nameof Request) kind

    let matchPathComponents (expectedRelativePath: string) (relativePath: string) =
        let expectedComponents = expectedRelativePath.TrimStart('/').Split('/')
        let actualComponents = relativePath.Split('/')

        if expectedComponents.Length <> actualComponents.Length then
            None
        else
            Seq.fold2
                (fun (acc: OrderedMap.Builder<string, string> option) (expected: string) (actual: string) ->
                    match acc with
                    | None -> None
                    | Some acc ->
                        if expected.StartsWith("{") && expected.EndsWith("}") then
                            let actual = WebUtility.UrlDecode(actual)
                            acc.Add(expected.Substring(1, expected.Length - 2), actual) |> Some
                        elif expected = actual then
                            Some acc
                        else
                            None)
                (Some(OrderedMap.Builder()))
                expectedComponents
                actualComponents
            |> Option.map _.Build()

    member val Kind = kind with get
    member val Inputs = inputs with get

    override this.ProcessInputs context =
        this.Inputs |> Option.map (_.ToJson() >> context.EvaluateLanguage)

    override this.RunFromRequest request context =
        let method = request.method
        let relativePath = request.relativePath

        let expectedMethod =
            match this.Inputs with
            | Some { method = Some m } ->
                m
                |> String
                |> context.EvaluateLanguage
                |> Conversions.ensureString
                |> _.ToUpperInvariant()
                |> Some
            | _ -> None

        let expectedRelativePath =
            match this.Inputs with
            | Some { relativePath = Some p } ->
                p |> String |> context.EvaluateLanguage |> Conversions.ensureString |> Some
            | _ -> None

        match expectedMethod with
        | Some m when m <> method.Method ->
            { ActionResult.Default with
                status = Failed }
        | _ ->
            let matches =
                match expectedRelativePath with
                | Some expectedRelativePath -> matchPathComponents expectedRelativePath relativePath
                | _ -> Some OrderedMap.empty

            match matches with
            | Some matches ->
                let processedInputs =
                    match inputs with
                    | Some _ ->
                        Some
                            { HttpRequestTriggerInputs.method = expectedMethod
                              relativePath = expectedRelativePath }
                    | None -> None

                let matchOutputs = if OrderedMap.isEmpty matches then None else Some matches

                let outputs =
                    { relativePathParameters = matchOutputs
                      queries = request.queries
                      headers = request.headers
                      body = request.body }

                { ActionResult.Default with
                    status = Succeeded
                    inputs = processedInputs |> Option.map _.ToJson()
                    outputs = Some(outputs.ToJson()) }
            | None ->
                { ActionResult.Default with
                    status = Failed }

// Actions

// Control actions

type Scope(resolveAction, json) =
    inherit BaseAction(json)

    member val Actions = JsonTree.getKey "actions" json |> actionGraphOfJson resolveAction with get

    override this.Execute (name: string) (context: SimulatorContext) =
        use scopeContext = context.PushScopeContext name false (this.GetChildren())

        let result = scopeContext.Run this.Actions in

        let code, error = codeAndErrorFromScopeResult result

        { ActionResult.Default with
            status = result
            code = Some code
            error = error }

    override this.GetChildren() = this.Actions |> OrderedMap.toSeq

type If(resolveAction, json) =
    inherit Scope(resolveAction, json)

    member val Expression = JsonTree.getKey "expression" json with get
    member val Else = JsonTree.getKey "else" json |> actionGraphContainerOfJson resolveAction with get

    override this.Execute (name: string) (context: SimulatorContext) =
        use scopeContext = context.PushScopeContext name false (this.GetChildren())

        let conditionResult =
            this.Expression |> context.EvaluateLanguage |> context.EvaluateCondition

        printfn "If Condition: %b" conditionResult

        let actions = if conditionResult then this.Actions else this.Else.actions

        let result = scopeContext.Run actions

        let code, error = codeAndErrorFromScopeResult result

        { status = result
          inputs = Some(Conversions.createObject [ "expressionResult", Boolean conditionResult ])
          outputs = None
          code = Some code
          error = error }

    override this.GetChildren() =
        Seq.append (OrderedMap.toSeq this.Actions) (OrderedMap.toSeq this.Else.actions)

type ForEach(resolveAction, json) =
    inherit Scope(resolveAction, json)

    member val ForEach = JsonTree.getKey "foreach" json with get

    override this.Execute (name: string) (context: SimulatorContext) =
        use scopeContext = context.PushScopeContext name true (this.GetChildren())

        printfn "ForEach: %s" (Conversions.prettyStringOfJson this.ForEach)

        let elems = this.ForEach |> context.EvaluateLanguage

        use loopContext =
            elems |> Conversions.ensureArray |> context.PushArrayOperationContext(Some name)

        let rec executeInnerScope () =
            if loopContext.Advance() then
                scopeContext.Run this.Actions |> ignore
                executeInnerScope ()

        executeInnerScope ()

        let result = scopeContext.OverallResult
        let code, error = codeAndErrorFromScopeResult result

        { ActionResult.Default with
            status = scopeContext.OverallResult
            inputs = Some(elems)
            code = Some code
            error = error }

type Switch(resolveAction, json) =
    inherit BaseAction(json)

    member val Expression = JsonTree.getKey "expression" json with get
    member val Default = JsonTree.getKey "default" json |> actionGraphContainerOfJson resolveAction with get

    member val Cases =
        JsonTree.getKey "cases" json
        |> Conversions.ensureObject
        |> OrderedMap.mapValuesOnly (switchCaseOfJson resolveAction) with get

    override this.Execute (_: string) (context: SimulatorContext) =
        use scopeContext =
            context.PushScopeContext this.ActionType false (this.GetChildren())

        let value = context.EvaluateLanguage this.Expression
        printfn "Switch Value: %O" value

        let actions =
            this.Cases.Values
            |> Seq.tryFind (fun case -> case.case = value)
            |> Option.map _.actions
            |> Option.defaultValue this.Default.actions

        let result = scopeContext.Run actions

        let code, error = codeAndErrorFromScopeResult result

        { status = result
          inputs = None
          outputs = Some(Conversions.createObject [ "expression", value ])
          code = Some code
          error = error }

    override this.GetChildren() =
        OrderedMap.toSeq this.Default.actions
        |> Seq.append (this.Cases.Values |> Seq.collect (fun c -> OrderedMap.toSeq c.actions))

type Until(resolveAction, json) =
    inherit Scope(resolveAction, json)

    member val Expression = JsonTree.getKey "expression" json with get
    member val Limit = JsonTree.getKey "limit" json |> untilLimitOfJson with get

    override this.Execute (_: string) (context: SimulatorContext) =
        use scopeContext =
            context.PushScopeContext this.ActionType true (this.GetChildren())

        let parsedTimeout = XmlConvert.ToTimeSpan(this.Limit.timeout)
        let timeout = System.DateTime.Now.Add(parsedTimeout)

        let rec attempt num =
            printfn "Until Attempt %d" num
            scopeContext.Run this.Actions |> ignore

            if System.DateTime.Now >= timeout then
                printfn "Until Timeout"
                scopeContext.MergeResult TimedOut
            else if num >= this.Limit.count then
                printfn "Until Limit"
            else
                let condition =
                    context.EvaluateLanguage(this.Expression) |> Conversions.ensureBoolean

                printfn "Until Condition: %b" condition

                if not condition then
                    attempt (num + 1L)

        attempt 1L

        let result = scopeContext.OverallResult
        let code, error = codeAndErrorFromScopeResult result

        { status = result
          inputs = None // TODO
          outputs = None // TODO
          code = Some code
          error = error }

type Terminate(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> terminateInputsOfJson with get

    override this.Execute (_: string) (context: SimulatorContext) =
        printfn "Terminate: %s (%O)" this.Inputs.runStatus this.Inputs.runError

        let processedInputs =
            this.Inputs
            |> jsonOfTerminateInputs
            |> context.EvaluateLanguage
            |> terminateInputsOfJson

        context.Terminate (statusOfString processedInputs.runStatus) processedInputs.runError

        { ActionResult.Default with
            inputs = Some(jsonOfTerminateInputs processedInputs)
            code = Some NotSpecified }

// Variable actions

type InitializeVariable(json) =
    inherit BaseAction(json)

    let validateJustOneVariable (v: InitializeVariableSingle VariablesInputs) =
        if v.variables.IsEmpty then
            failwith "No variables to initialize in InitializeVariable"

        if not v.variables.Tail.IsEmpty then
            failwith "Only one variable can be initialized at a time with InitializeVariable"

        v

    member val Inputs =
        JsonTree.getKey "inputs" json
        |> variablesInputsOfJson initializeVariableSingleOfJson
        |> validateJustOneVariable with get

    override this.Execute (_: string) (context: SimulatorContext) =
        let firstVar = this.Inputs.variables.Head

        printfn
            "InitializeVariable: %s (%O) = %s"
            firstVar.name
            firstVar.type_
            (Conversions.prettyStringOfJson firstVar.value)

        let processedVars =
            this.Inputs.variables
            |> List.map (fun variable ->
                let value = variable.value |> context.EvaluateLanguage |> coerce variable.type_ in

                (variable.name, variable.type_, value))

        processedVars
        |> List.iter (fun (name, _, value) ->
            printfn "InitializeVariable: %s = %s" name (Conversions.prettyStringOfJson value)

            if context.GetVariable name |> Option.isSome then
                failwithf "Variable '%s' already exists" name

            context.SetVariable name value)

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

    let typecheck (originalType: VariableType) (newValue: JsonTree) =
        match originalType, getVarType newValue with
        | VariableType.Float, VariableType.Integer -> ()
        | a, b when a = b -> ()
        | VariableType.String, VariableType.Object when newValue = Null -> ()
        | VariableType.Array, VariableType.Object when newValue = Null -> ()
        | a, b -> failwithf "Variable is of type %O, cannot set to %O: %s" a b (Conversions.stringOfJson newValue)

    member val Inputs = JsonTree.getKey "inputs" json |> setVariableSingleOfJson with get

    override this.Execute (_: string) (context: SimulatorContext) =
        printfn "SetVariable: %s = %O" this.Inputs.name (Conversions.prettyStringOfJson this.Inputs.value)

        match context.GetVariable this.Inputs.name with
        | None -> failwithf "Variable '%s' does not exist" this.Inputs.name
        | Some originalValue ->
            let value = this.Inputs.value |> context.EvaluateLanguage

            let originalType = getVarType originalValue
            typecheck originalType value

            context.SetVariable this.Inputs.name <| coerce originalType value

            let inputs =
                Conversions.createObject [ "name", String this.Inputs.name; "value", value ]

            { ActionResult.Default with
                inputs = Some(inputs)
                outputs = Some(Conversions.createObject [ "body", inputs ])
                code = Some NotSpecified }

type AppendToStringVariable(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> setVariableSingleOfJson with get

    override this.Execute (_: string) (context: SimulatorContext) =
        printfn "%s: %s = %s" this.ActionType this.Inputs.name (Conversions.prettyStringOfJson this.Inputs.value)

        match context.GetVariable this.Inputs.name with
        | None -> failwithf "Variable '%s' does not exist" this.Inputs.name
        | Some originalValue ->
            let addend = this.Inputs.value |> context.EvaluateLanguage
            let newValue = this.Add context originalValue addend
            context.SetVariable this.Inputs.name newValue

            { ActionResult.Default with
                inputs = Some(Conversions.createObject [ "name", String this.Inputs.name; "value", this.Inputs.value ])
                code = Some NotSpecified }

    abstract member Add: SimulatorContext -> JsonTree -> JsonTree -> JsonTree

    override this.Add _ a b =
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

    override this.Execute (_: string) (context: SimulatorContext) =
        printfn "%s: %s = %s" this.ActionType this.Inputs.name (Conversions.prettyStringOfJson this.Inputs.value)

        let originalValue =
            getVarTypechecked context this.Inputs.name [ VariableType.Integer; VariableType.Float ]

        let increment = this.Inputs.value |> context.EvaluateLanguage

        let value =
            this.Add (originalValue |> Conversions.ensureInteger) (increment |> Conversions.ensureInteger)

        context.SetVariable this.Inputs.name (Integer value)

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

    override this.Execute (_: string) (context: SimulatorContext) =
        printfn "Compose: %s" (Conversions.prettyStringOfJson this.Inputs)
        let result = context.EvaluateLanguage this.Inputs
        printfn "Compose Result: %s" (Conversions.prettyStringOfJson result)

        { ActionResult.Default with
            inputs = Some(result)
            outputs = Some(result)
            code = Some OK }

type ParseJson(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> parseJsonInputsOfJson with get

    override this.Execute (_: string) (context: SimulatorContext) =
        printfn "ParseJson: %O" this.Inputs
        let evaluatedContent = context.EvaluateLanguage this.Inputs.content
        let evaluatedSchema = context.EvaluateLanguage this.Inputs.schema

        let result =
            match evaluatedContent with
            | String input -> input |> Parser.parse
            | json -> json

        printfn "ParseJson Result: %s" (Conversions.prettyStringOfJson result)

        let parsedSchema = SchemaValidator.jsonSchemaOfJson evaluatedSchema
        let validationResult = SchemaValidator.validateJsonSchema parsedSchema result

        { ActionResult.Default with
            status = if validationResult then Succeeded else Failed
            inputs = Some(Conversions.createObject [ "content", evaluatedContent; "schema", evaluatedSchema ])
            outputs = Some(Conversions.createObject [ "body", result ])
            code = Some OK }

type Query(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> queryInputsOfJson with get

    override this.Execute (name: string) (context: SimulatorContext) =
        printfn
            "Query: %s in %s"
            (Conversions.prettyStringOfJson this.Inputs.where)
            (Conversions.prettyStringOfJson this.Inputs.from)

        let from = context.EvaluateLanguage this.Inputs.from

        let arrayVals = from |> Conversions.ensureArray
        let nameOpt = if context.IsBugForBugAccurate then None else Some name
        use loopContext = context.PushArrayOperationContext nameOpt arrayVals

        let rec filterValsRev acc =
            if loopContext.Advance() then
                let current = loopContext.Current

                let condition =
                    this.Inputs.where |> context.EvaluateLanguage |> Conversions.ensureBoolean

                let next = if condition then current :: acc else acc

                filterValsRev next
            else
                acc

        let result = filterValsRev [] |> List.rev |> Conversions.createArray in

        printfn "Query Result: %s" (Conversions.prettyStringOfJson result)

        { ActionResult.Default with
            inputs = Some(from)
            outputs = Some(Conversions.createObject [ "body", result ]) }

type Select(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> selectInputsOfJson with get

    override this.Execute (name: string) (context: SimulatorContext) =
        printfn
            "Select: %s in %s"
            (Conversions.prettyStringOfJson this.Inputs.select)
            (Conversions.prettyStringOfJson this.Inputs.from)

        let from = context.EvaluateLanguage this.Inputs.from

        let arrayVals = from |> Conversions.ensureArray
        let nameOpt = if context.IsBugForBugAccurate then None else Some name
        use loopContext = context.PushArrayOperationContext nameOpt arrayVals

        let rec selectValsRev acc =
            if loopContext.Advance() then
                let selected = this.Inputs.select |> context.EvaluateLanguage
                selectValsRev (selected :: acc)
            else
                acc

        let result = selectValsRev [] |> List.rev |> Conversions.createArray in

        printfn "Select Result: %s" (Conversions.prettyStringOfJson result)

        { ActionResult.Default with
            inputs = Some(from)
            outputs = Some(Conversions.createObject [ "body", result ]) }

// Inline Code actions

type JavaScriptCode(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> javaScriptCodeInputsOfJson with get

    override this.Execute (_: string) (context: SimulatorContext) =
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

    override this.Execute (name: string) (context: SimulatorContext) =
        printfn "ServiceProvider: %O" this.Inputs

        let result = ref HttpRequestReply.Default

        let processedInputs =
            { this.Inputs with
                parameters = context.EvaluateLanguage(this.Inputs.parameters) }

        context.ExternalServiceRequest
        <| ExternalServiceTypes.ServiceProvider(
            { actionName = name
              parameters = processedInputs.parameters |> context.EvaluateLanguage
              serviceProviderConfiguration = processedInputs.serviceProviderConfiguration },
            result
        )

        let success = httpStatusCodeIsSuccess result.Value.statusCode

        { ActionResult.Default with
            status = if success then Succeeded else Failed
            inputs = Some(jsonOfServiceProviderInputs processedInputs)
            outputs = Some(jsonOfHttpRequestReply result.Value) }

// Local Function Operations actions

type InvokeFunction(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> InvokeFunctionInputs.ofJson with get

    override this.Execute (name: string) (context: SimulatorContext) =
        let result = ref HttpRequestReply.Default

        let processedInputs =
            { functionName =
                this.Inputs.functionName
                |> String
                |> context.EvaluateLanguage
                |> Conversions.ensureString
              parameters = context.EvaluateLanguage(this.Inputs.parameters) }

        context.ExternalServiceRequest
        <| ExternalServiceTypes.InvokeFunction(
            { actionName = name
              functionName = processedInputs.functionName
              parameters = processedInputs.parameters },
            result
        )

        let success = httpStatusCodeIsSuccess result.Value.statusCode

        { ActionResult.Default with
            status = if success then Succeeded else Failed
            inputs = Some(Conversions.createObject [ "body", processedInputs.ToJson() ])
            outputs = Some(jsonOfHttpRequestReply result.Value) }

// Request actions

type Response(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> httpResponseInputsOfJson with get

    override this.Execute (_: string) (context: SimulatorContext) =
        printfn "Response: %O" this.Inputs

        let inline stringPairSeqToObject seq =
            Seq.map (fun (k, v) -> k, String v) seq |> Conversions.createObject

        let inline parseIntegerOrStringteger json =
            match json with
            | String s -> int s
            | Integer i -> int i
            | _ -> failwith "Expected integer or string"

        let inline expect2xx4xx5xx i =
            if i < 200 || i >= 600 || (i >= 300 && i < 400) then
                failwith "Expected 2xx, 4xx, or 5xx status code"

            i

        let processedBody = this.Inputs.body |> context.EvaluateLanguage

        let processedHeaders =
            this.Inputs.headers
            |> Option.map (
                Seq.map (fun (KeyValue(k, v)) ->
                    k, (v |> String |> context.EvaluateLanguage |> Conversions.rawStringOfJson))
            )

        let processedStatusCode = context.EvaluateLanguage(this.Inputs.statusCode)

        let actualHeaders =
            // TODO: allow the toggle that disables the default headers
            processedHeaders
            |> Option.defaultValue Seq.empty
            |> OrderedMap.Builder().AddRange
            |> addWorkflowResponseHeadersToBuilder context
            |> _.Build()
            |> Some

        context.ExternalServiceRequest
        <| HttpResponse(
            { HttpRequestReply.statusCode = processedStatusCode |> parseIntegerOrStringteger |> expect2xx4xx5xx
              headers = actualHeaders
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

    override this.Execute (name: string) (context: SimulatorContext) =
        printfn "HTTP: %O" this.Inputs

        let result = ref HttpRequestReply.Default in

        let evaluateString s =
            s |> String |> context.EvaluateLanguage |> Conversions.ensureString

        let evaluateStringStringMap map =
            map |> OrderedMap.map (fun k v -> evaluateString k, evaluateString v)

        let processedInputs =
            { method = evaluateString this.Inputs.method
              uri = evaluateString this.Inputs.uri
              headers = this.Inputs.headers |> Option.map evaluateStringStringMap
              body = this.Inputs.body |> context.EvaluateLanguage
              queries = this.Inputs.queries |> Option.map evaluateStringStringMap
              cookie = this.Inputs.cookie |> Option.map evaluateString
              authentication = this.Inputs.authentication |> context.EvaluateLanguage
              retryPolicy = this.Inputs.retryPolicy |> Option.map context.EvaluateLanguage }

        let headers =
            processedInputs.headers
            |> Option.map (OrderedMap.Builder().AddRange)
            |> Option.defaultValue (OrderedMap.Builder())

        let processedContent = contentOfJson processedInputs.body

        // Add content-type if missing
        // TODO case-insensitivity
        processedContent
        |> Option.iter (fun (type_, _) -> headers.TryAdd("Content-Type", type_) |> ignore)

        // Add extra headers
        // TODO toggle if requested in json
        addWorkflowHttpRequestHeadersToBuilder name context headers |> ignore

        context.ExternalServiceRequest
        <| HttpRequest(
            { method = processedInputs.method
              uri = processedInputs.uri
              headers = headers.Build()
              body = Option.map snd processedContent
              queryParameters = processedInputs.queries |> Option.defaultValue OrderedMap.empty
              cookie = processedInputs.cookie
              authentication = processedInputs.authentication },
            result
        )

        let success = httpStatusCodeIsSuccess result.Value.statusCode
        // TODO: retry policy

        { ActionResult.Default with
            status = if success then Succeeded else Failed
            inputs = Some(jsonOfHttpInputs processedInputs)
            outputs = Some(jsonOfHttpRequestReply result.Value) }

// Workflow actions

type Workflow(json) =
    inherit BaseAction(json)

    member val Inputs = JsonTree.getKey "inputs" json |> workflowRequestOfJson with get

    member val OperationOptions =
        JsonTree.tryGetKey "operationOptions" json
        |> Option.map Conversions.ensureString with get

    override this.Execute (name: string) (context: SimulatorContext) =
        printfn "Workflow: %s" (jsonOfWorkflowRequest this.Inputs |> Conversions.prettyStringOfJson)

        let headers =
            this.Inputs.headers
            |> OrderedMap.map (fun k v ->
                context.EvaluateLanguage(String k) |> Conversions.ensureString, context.EvaluateLanguage(v))

        let body = this.Inputs.body |> context.EvaluateLanguage

        let asyncSupported =
            not (this.OperationOptions |> Option.exists ((=) "DisableAsyncPattern"))

        let result =
            ref
                { HttpRequestReply.Default with
                    statusCode = System.Int32.MinValue }

        let request =
            { actionName = name
              workflowId = this.Inputs.workflowId
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
                status =
                    if httpStatusCodeIsSuccess result.Value.statusCode then
                        Succeeded
                    else
                        Failed
                inputs = Some(jsonOfWorkflowRequest request)
                outputs = Some(jsonOfHttpRequestReply result.Value)
                code = Some Accepted }
