namespace IllogicApps.Core.LogicAppActions

open System.Collections.Immutable
open System.Xml

open IllogicApps.Core
open IllogicApps.Core.LogicAppBaseAction
open IllogicApps.Core.LogicAppSpec
open IllogicApps.Core.LogicAppActionSupport
open CompletedStepTypes
open ExternalServiceTypes
open IllogicApps.Json

// Triggers

type Request() =
    inherit BaseAction()

    member val Kind = "" with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Trigger: %s" this.Kind

        // TODO: Does this ever get called?

        let triggerResult = context.TriggerResult

        { status = triggerResult.action.status
          inputs = triggerResult.action.inputs
          outputs = triggerResult.action.outputs }

    static member OfJson json =
        let r = Request()
        r.Kind <- JsonTree.getKey "kind" json |> Conversions.ensureString
        r.AugmentWithJson json
        r

// Actions

// Control actions

type Scope() =
    inherit BaseAction()

    member val Actions: ActionGraph = OrderedMap.empty with get, set

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

    static member OfJson resolveAction json =
        let r = Scope()
        r.Actions <- JsonTree.getKey "actions" json |> actionGraphOfJson resolveAction
        r.AugmentWithJson json
        r

type If() =
    inherit Scope()

    member val Expression = defaultExpression () with get, set
    member val Else = ActionGraphContainer.Default with get, set

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

        { status = result
          inputs = None
          outputs = Some(Conversions.createObject [ "expression", Boolean conditionResult ]) }

    override this.GetChildren() : (string * IGraphExecutable) list =
        Seq.append this.Actions this.Else.actions
        |> Seq.map (fun kv -> kv.Key, (kv.Value: IGraphExecutable))
        |> Seq.toList

    static member OfJson resolveAction json =
        let r = If()
        r.Actions <- JsonTree.getKey "actions" json |> actionGraphOfJson resolveAction
        r.Else <- JsonTree.getKey "else" json |> actionGraphContainerOfJson resolveAction
        r.Expression <- JsonTree.getKey "expression" json
        r.AugmentWithJson json
        r


type Switch() =
    inherit BaseAction()

    member val Expression: JsonTree = Null with get, set
    member val Default = ActionGraphContainer.Default with get, set
    member val Cases: OrderedMap<string, SwitchCase> = OrderedMap.empty with get, set

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

        { status = result
          inputs = None
          outputs = Some(Conversions.createObject [ "expression", value ]) }

    override this.GetChildren() =
        this.Cases.Values
        |> Seq.fold (fun acc case -> Seq.append acc case.actions) this.Default.actions
        |> Seq.map (fun kv -> kv.Key, (kv.Value: IGraphExecutable))
        |> Seq.toList

    static member OfJson resolveAction json =
        let r = Switch()
        r.Expression <- JsonTree.getKey "expression" json
        r.Default <- JsonTree.getKey "default" json |> actionGraphContainerOfJson resolveAction

        r.Cases <-
            JsonTree.getKey "cases" json
            |> Conversions.ensureObject
            |> OrderedMap.mapValuesOnly (switchCaseOfJson resolveAction)

        r.AugmentWithJson json
        r

type Until() =
    inherit Scope()

    member val Expression: JsonTree = Null with get, set
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

        { status = result
          inputs = None // TODO
          outputs = None // TODO
        }

    static member OfJson resolveAction json =
        let r = Until()
        r.Actions <- JsonTree.getKey "actions" json |> actionGraphOfJson resolveAction
        r.Expression <- JsonTree.getKey "expression" json
        r.Limit <- JsonTree.getKey "limit" json |> untilLimitOfJson
        r.AugmentWithJson json
        r

type Terminate() =
    inherit BaseAction()

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
          inputs = Some(jsonOfTerminateInputs this.Inputs)
          outputs = None }

    static member OfJson json =
        let r = Terminate()
        r.Inputs <- JsonTree.getKey "inputs" json |> terminateInputsOfJson
        r.AugmentWithJson json
        r

// Variable actions

type InitializeVariable() =
    inherit BaseAction()

    member val Inputs: InitializeVariableSingle VariablesInputs = { variables = [] } with get, set

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
            |> ImmutableArray.CreateRange
            |> Array

        { status = Succeeded
          inputs = Some(Conversions.createObject [ "variables", processedVarsArray ])
          outputs = None }

    static member OfJson json =
        let r = InitializeVariable()

        r.Inputs <-
            JsonTree.getKey "inputs" json
            |> variablesInputsOfJson initializeVariableSingleOfJson

        r.AugmentWithJson json
        r

type SetVariable() =
    inherit BaseAction()

    let typecheck (originalValue: JsonTree) (newValue: JsonTree) =
        if getVarType originalValue <> getVarType newValue then
            failwithf "Variable is of type %A, cannot set to %A" (getVarType originalValue) (getVarType newValue)

    member val Inputs = { name = ""; value = Null } with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "SetVariable: %s = %O" this.Inputs.name this.Inputs.value

        if not (context.Variables.ContainsKey(this.Inputs.name)) then
            failwithf "Variable '%s' does not exist" this.Inputs.name

        typecheck context.Variables.[this.Inputs.name] this.Inputs.value

        let value = this.Inputs.value |> context.EvaluateLanguage

        context.Variables.[this.Inputs.name] <- value

        let inputs =
            Conversions.createObject [ "name", String this.Inputs.name; "value", value ]

        { status = Succeeded
          inputs = Some(inputs)
          outputs = Some(Conversions.createObject [ "body", inputs ]) }

    static member OfJson json =
        let r = SetVariable()
        r.Inputs <- JsonTree.getKey "inputs" json |> setVariableSingleOfJson
        r.AugmentWithJson json
        r


type AppendToStringVariable() =
    inherit BaseAction()

    member val Inputs = { name = ""; value = Null } with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "%s: %s = %O" this.ActionType this.Inputs.name this.Inputs.value

        if not (context.Variables.ContainsKey(this.Inputs.name)) then
            failwithf "Variable '%s' does not exist" this.Inputs.name

        let originalValue = context.Variables[this.Inputs.name]
        let addend = this.Inputs.value |> context.EvaluateLanguage
        let newValue = this.Add context originalValue addend
        context.Variables.[this.Inputs.name] <- newValue

        { status = Succeeded
          inputs = Some(Conversions.createObject [ "name", String this.Inputs.name; "value", this.Inputs.value ])
          outputs = None }

    abstract member Add: SimulatorContext -> JsonTree -> JsonTree -> JsonTree

    override this.Add context a b =
        String((Conversions.ensureString a) + (Conversions.ensureString b))

    static member OfJson json =
        let r = AppendToStringVariable()
        r.Inputs <- JsonTree.getKey "inputs" json |> setVariableSingleOfJson
        r.AugmentWithJson json
        r

type AppendToArrayVariable() =
    inherit AppendToStringVariable()

    override this.Add context a b =
        if context.IsBugForBugAccurate then
            match b with
            | Array _ -> failwith "Cannot append an array to an array"
            | Null -> failwith "Cannot append null to an array"
            | _ -> ()

        Conversions.ensureArray a |> _.Add(b) |> Array

    static member OfJson json =
        let r = AppendToArrayVariable()
        r.Inputs <- JsonTree.getKey "inputs" json |> setVariableSingleOfJson
        r.AugmentWithJson json
        r

type IncrementVariable() =
    inherit SetVariable()

    override this.Execute(context: SimulatorContext) =
        printfn "%s: %s" this.ActionType this.Inputs.name

        let originalValue =
            getVarTypechecked context this.Inputs.name [ VariableType.Integer; VariableType.Float ]

        let increment = this.Inputs.value |> context.EvaluateLanguage

        let value =
            this.Add (originalValue |> Conversions.ensureInteger) (increment |> Conversions.ensureInteger)

        context.Variables.[this.Inputs.name] <- Integer value

        { status = Succeeded
          inputs =
            Some(
                Conversions.createObject
                    [ "body", Conversions.createObject [ "name", String this.Inputs.name; "value", increment ] ]
            )
          outputs = Some(Conversions.createObject [ "name", String this.Inputs.name; "value", Integer value ]) }

    abstract member Add: int64 -> int64 -> int64
    override this.Add a b = a + b

    static member OfJson json =
        let r = IncrementVariable()
        r.Inputs <- JsonTree.getKey "inputs" json |> setVariableSingleOfJson
        r.AugmentWithJson json
        r

type DecrementVariable() =
    inherit IncrementVariable()

    override this.Add a b = a - b

    static member OfJson json =
        let r = DecrementVariable()
        r.Inputs <- JsonTree.getKey "inputs" json |> setVariableSingleOfJson
        r.AugmentWithJson json
        r

// Data Operations actions

type Compose() =
    inherit BaseAction()

    member val Inputs: JsonTree = Null with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Compose: %O" this.Inputs
        let result = context.EvaluateLanguage this.Inputs
        printfn "Compose Result: %O" result

        { status = Succeeded
          inputs = Some(result)
          outputs = Some(result) }

    static member OfJson json =
        let r = Compose()
        r.Inputs <- JsonTree.getKey "inputs" json
        r.AugmentWithJson json
        r

type ParseJson() =
    inherit BaseAction()

    member val Inputs: ParseJsonInputs = { content = Null; schema = Null } with get, set

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

        { status = Succeeded
          inputs = Some(Conversions.createObject [ "content", input; "schema", this.Inputs.schema ])
          outputs = Some(Conversions.createObject [ "body", result ]) }

    static member OfJson json =
        let r = ParseJson()
        r.Inputs <- JsonTree.getKey "inputs" json |> parseJsonInputsOfJson
        r.AugmentWithJson json
        r

type Query() =
    inherit BaseAction()

    member val Inputs: QueryInputs = { from = Null; where = Null } with get, set

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

        { status = Succeeded
          inputs = Some(from)
          outputs = Some(Conversions.createObject [ "body", Conversions.createArray result ]) }

    static member OfJson json =
        let r = Query()
        r.Inputs <- JsonTree.getKey "inputs" json |> queryInputsOfJson
        r.AugmentWithJson json
        r

// Inline Code actions

type JavaScriptCode() =
    inherit BaseAction()

    // TODO: correct inputs
    member val Inputs: JsonTree = Null with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "NOT IMPLEMENTED JavaScriptCode: %O" this.Inputs

        { status = Failed
          inputs = None
          outputs = None }

    static member OfJson json =
        let r = JavaScriptCode()
        r.Inputs <- JsonTree.getKey "inputs" json
        r.AugmentWithJson json
        r

// Request actions

type Response() =
    inherit BaseAction()

    member val Inputs =
        { HttpResponseInputs.body = Null
          headers = None
          statusCode = Integer 0 } with get, set

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
              headers =
                (processedHeaders
                 |> Option.map OrderedMap.ofSeq
                 |> Option.defaultValue OrderedMap.empty)
              body = processedBody }
        )

        let inputsObject =
            OrderedMap
                .Builder()
                .Add("statusCode", processedStatusCode)
                .MaybeAdd("body", processedBody)
                .MaybeAdd("headers", Option.map stringPairSeqToObject processedHeaders)
                .Build()
            |> Object

        { status = Succeeded
          inputs = Some(inputsObject)
          outputs = Some(inputsObject) }

    static member OfJson json =
        let r = Response()
        r.Inputs <- JsonTree.getKey "inputs" json |> httpResponseInputsOfJson
        r.AugmentWithJson json
        r

// HTTP actions

type Http() =
    inherit BaseAction()

    member val Inputs: HttpInputs =
        { method = ""
          uri = ""
          headers = None
          body = Null
          queries = None
          cookie = None
          authentication = Null } with get, set

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

        { status = Succeeded
          inputs = Some(jsonOfHttpInputs this.Inputs)
          outputs = Some(jsonOfHttpRequestReply result.Value) }

    static member OfJson json =
        let r = Http()
        r.Inputs <- JsonTree.getKey "inputs" json |> httpInputsOfJson
        r.AugmentWithJson json
        r

// Workflow actions

type Workflow() =
    inherit BaseAction()

    member val Inputs =
        { workflowId = ""
          headers = OrderedMap.empty
          body = Null
          retryPolicy = WorkflowRequestRetryPolicy.Default } with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Workflow: %s" (jsonOfWorkflowRequest this.Inputs |> Conversions.stringOfJson)

        let headers =
            this.Inputs.headers
            |> OrderedMap.map (fun k v ->
                context.EvaluateLanguage(String k) |> Conversions.ensureString,
                context.EvaluateLanguage(String v) |> Conversions.ensureString)

        let body = this.Inputs.body |> context.EvaluateLanguage

        let result = ref HttpRequestReply.Default

        let request =
            { workflowId = this.Inputs.workflowId
              headers = headers
              body = body
              retryPolicy = this.Inputs.retryPolicy }

        context.ExternalServiceRequest <| ExternalServiceTypes.Workflow(request, result)

        { status = Succeeded
          inputs = Some(jsonOfWorkflowRequest request)
          outputs = Some(jsonOfHttpRequestReply result.Value) }

    static member OfJson json =
        let r = Workflow()
        r.Inputs <- JsonTree.getKey "inputs" json |> workflowRequestOfJson
        r.AugmentWithJson json
        r
