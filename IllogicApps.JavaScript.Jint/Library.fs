module IllogicApps.JavaScript.Jint

open IllogicApps.Core
open IllogicApps.Core.ExternalServiceTypes
open IllogicApps.Json
open global.Jint
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

let rec jsonOfJsValue (value: JsValue) : JsonTree =
    if value.IsString() then
        JsonTree.String(value.AsString())
    elif value.IsNumber() then
        JsonTree.Float(value.AsNumber())
    elif value.IsBoolean() then
        JsonTree.Boolean(value.AsBoolean())
    elif value.IsNull() || value.IsUndefined() then
        JsonTree.Null
    elif value.IsObject() then
        value.AsObject().GetOwnProperties()
        |> Seq.filter (fun (KeyValue(_, value)) -> value.Enumerable)
        |> Seq.map (fun (KeyValue(key, value)) -> key.AsString(), jsonOfJsValue value.Value)
        |> Conversions.createObject
    elif value.IsArray() then
        value.AsArray() |> Seq.map jsonOfJsValue |> Conversions.createArray
    else
        failwithf "Unsupported JsValue: %A" value

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

        engine.SetValue("console", Jint.Console.makeConsoleObj engine) |> ignore

        result.Value <-
            try
                Ok(script |> engine.Evaluate |> jsonOfJsValue)
            with :? Jint.Runtime.JavaScriptException as e ->
                Error(e.Message)

        true
    | _ -> false
