module IllogicApps.JavaScript.Jint.Handler

open IllogicApps.Core
open IllogicApps.Core.ExternalServiceTypes
open IllogicApps.Json
open Jint
open Jint.Native
open CompletedStepTypes

let rec jsValueOfJson engine (json: JsonTree) : JsValue =
    match json with
    | JsonTree.String s -> JsString(s)
    | JsonTree.Integer i -> JsNumber(i)
    | JsonTree.Float f -> JsNumber(f)
    | JsonTree.Decimal d -> JsNumber(float d)
    | JsonTree.Boolean b -> if b then JsBoolean.True else JsBoolean.False
    | JsonTree.Object o ->
        let obj = JsObject(engine)

        for KeyValue(k, v) in o do
            obj.FastSetDataProperty(k, jsValueOfJson engine v)

        obj
    | JsonTree.Array a ->
        a
        |> Seq.map (jsValueOfJson engine)
        |> Array.ofSeq
        |> fun items -> JsArray(engine, items)
    | JsonTree.Null -> JsValue.Null

[<Literal>]
let minIntAsFloat = -9223372036854775808.0

// At this point, Int64.MaxValue is too precise to be represented as a float
// to we use the value +1, which can be represented exactly.
[<Literal>]
let maxIntPlusOneAsFloat = 9223372036854775808.0

let numberOfFloat =
    function
    | v when System.Double.IsInteger(v) && v >= minIntAsFloat && v < maxIntPlusOneAsFloat -> Integer(int64 v)
    | v -> Float v

let rec jsonOfJsValue (value: JsValue) : JsonTree =
    match value with
    | value when value.IsString() -> JsonTree.String(value.AsString())
    | value when value.IsNumber() -> numberOfFloat (value.AsNumber())
    | value when value.IsBoolean() -> JsonTree.Boolean(value.AsBoolean())
    | value when value.IsNull() || value.IsUndefined() -> JsonTree.Null
    | value when value.IsArray() -> value.AsArray() |> Seq.map jsonOfJsValue |> Conversions.createArray
    | value when value.IsObject() ->
        value.AsObject().GetOwnProperties()
        |> Seq.filter (fun (KeyValue(_, value)) -> value.Enumerable)
        |> Seq.map (fun (KeyValue(key, value)) -> key.AsString(), jsonOfJsValue value.Value)
        |> Conversions.createObject
    | value -> failwithf "Unsupported JsValue: %A" value

let jintJavascriptHandler (_sim: SimulatorContext) (request: ExternalServiceRequest) =
    match request with
    | ScriptExecution({ language = ScriptingLanguage.JavaScript
                        source = ScriptSource.Inline script
                        actions = actions
                        trigger = trigger
                        workflow = workflow },
                      result) ->
        use engine = new Engine()

        engine.SetValue(
            "workflowContext",
            [ "actions", actions |> OrderedMap.mapValuesOnly jsonOfCompletedAction |> Object
              "trigger", trigger |> jsonOfCompletedTrigger
              "workflow", workflow |> jsonOfWorkflowDetails ]
            |> Conversions.createObject
            |> jsValueOfJson engine
        )
        |> ignore

        engine.SetValue("console", Console.makeConsoleObj engine) |> ignore

        result.Value <-
            try
                Ok(script |> engine.Evaluate |> jsonOfJsValue)
            with :? Jint.Runtime.JavaScriptException as e ->
                Error(e.Message)

        true
    | _ -> false
