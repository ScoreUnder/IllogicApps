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

    override this.GetChildren() : (string * BaseAction) list = this.Actions |> fromKvps |> Seq.toList

type If() =
    inherit Scope()

    member val Expression = defaultExpression () with get, set
    member val ElseActions: ActionGraph = Map.empty with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "If Begin"
        let conditionResult = context.EvaluateCondition this.Expression in // TODO
        printfn "If Condition: %b" conditionResult

        let actions = if conditionResult then this.Actions else this.ElseActions in

        let result = context.ExecuteGraph actions in
        printfn "If End"

        { status = result
          inputs = None
          outputs = Some(makeObject [ "expression", JsonValue.Create(conditionResult) ]) }

    override this.GetChildren() : (string * BaseAction) list =
        Seq.append this.Actions this.ElseActions |> fromKvps |> Seq.toList


type Switch() =
    inherit Action()

    member val Expression: JsonNode = JsonValue.Create(null) with get, set
    member val Default = new SwitchDefault() with get, set
    member val Cases: Map<string, SwitchCase> = Map.empty with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Switch Begin"
        let value = context.EvaluateLanguage this.Expression in
        printfn "Switch Value: %O" value

        let result =
            this.Cases.Values
            |> Seq.tryFind (fun case -> case.Case = value)
            |> Option.map (fun case -> case.Actions)
            |> Option.defaultValue this.Default.Actions
            |> context.ExecuteGraph in

        printfn "Switch End"

        { status = result
          inputs = None
          outputs = Some(makeObject [ "expression", value ]) }

    override this.GetChildren() : (string * BaseAction) list =
        this.Cases.Values
        |> Seq.fold (fun acc case -> Seq.append acc case.Actions) this.Default.Actions
        |> fromKvps
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

        context.Variables.[this.Inputs.Name] <- this.Inputs.Value

        { status = Succeeded
          inputs =
            Some(
                new JsonObject(
                    [ new KeyValuePair<string, JsonNode>("name", JsonValue.Create(this.Inputs.Name))
                      new KeyValuePair<string, JsonNode>("value", this.Inputs.Value) ]
                )
            )
          outputs = None }

type AppendToStringVariable() =
    inherit Action()

    member val Inputs = new SetVariableSingle() with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "AppendToStringVariable: %s = %O" this.Inputs.Name this.Inputs.Value

        if not (context.Variables.ContainsKey(this.Inputs.Name)) then
            failwithf "Variable '%s' does not exist" this.Inputs.Name

        let originalValue = context.Variables.[this.Inputs.Name]

        if originalValue.GetValueKind() <> JsonValueKind.String then
            failwithf "Variable is of type %A, cannot append to string" (originalValue.GetValueKind())

        let addend = this.Inputs.Value |> context.EvaluateLanguage

        let newValue = JsonValue.Create(originalValue.ToString() + addend.ToString())
        context.Variables.[this.Inputs.Name] <- newValue

        { status = Succeeded
          inputs = Some(makeObject [ ("name", JsonValue.Create(this.Inputs.Name)); ("value", this.Inputs.Value) ])
          // TODO check this for accuracy
          outputs = Some(makeObject [ ("name", JsonValue.Create(this.Inputs.Name)); ("value", newValue) ]) }

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

type Response() =
    inherit Action()

    member val Inputs =
        { body = None
          headers = None
          statusCode = 0 } with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Response: %O" this.Inputs

        let inputsObject =
            new JsonObject(
                let inline addKeyValuePair
                    (key: string)
                    (value: JsonNode option)
                    (seq: seq<KeyValuePair<string, JsonNode>>)
                    =
                    value
                    |> Option.map (fun v -> new KeyValuePair<string, JsonNode>(key, v))
                    |> Option.toList
                    |> Seq.append seq

                [ new KeyValuePair<string, JsonNode>("statusCode", JsonValue.Create(this.Inputs.statusCode)) ]
                |> addKeyValuePair "body" this.Inputs.body
                |> addKeyValuePair
                    "headers"
                    (this.Inputs.headers
                     |> Option.map (fun h ->
                         h
                         |> Seq.map (fun kv -> kv.Key, (JsonValue.Create(kv.Value): JsonNode))
                         |> makeObject))
            )

        { status = Succeeded
          inputs = Some(inputsObject |> context.EvaluateLanguage)
          outputs = None }
