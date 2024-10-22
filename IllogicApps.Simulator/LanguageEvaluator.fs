module IllogicApps.Simulator.LanguageEvaluator

open System.Text.Json.Nodes
open System.Text.Json

let accessMember (parent: JsonNode) (mem: JsonNode) =
    match parent.GetValueKind(), mem.GetValueKind() with
    | JsonValueKind.Object, JsonValueKind.String ->
        let propName = mem.GetValue<string>()

        parent.AsObject().TryGetPropertyValue(propName)
        |> function
            | true, value -> Ok value
            | _ -> Error(sprintf "Property %s not found" propName)
    | JsonValueKind.Object, kind -> failwithf "Cannot access property of object using %A" kind
    | JsonValueKind.Array, JsonValueKind.Number ->
        let index = mem.GetValue<int>()
        let arr = parent.AsArray()

        if index >= 0 && index < arr.Count then
            Ok(arr.[index])
        else
            Error(sprintf "Index %d out of bounds in array of length %d" index arr.Count)
    | JsonValueKind.Array, kind -> failwithf "Cannot index array with %A" kind
    | JsonValueKind.Null, _ -> Error "Cannot access property of null"
    | kind, _ -> failwithf "Cannot access property of %A" kind

let rec evaluate simContext (ast: LanguageParser.Ast) =
    match ast with
    | LanguageParser.Literal(lit) -> lit
    | LanguageParser.Call(name, args) ->
        match BuiltinFunctions.functions.TryGetValue(name) with
        | true, func -> args |> List.map (evaluate simContext) |> func simContext
        | _ -> failwithf "Function %s not found" name
    | LanguageParser.Member(parent, mem) ->
        accessMember (evaluate simContext parent) (evaluate simContext mem)
        |> Result.defaultWith (fun err -> failwith err)
    | LanguageParser.ForgivingMember(parent, mem) ->
        accessMember (evaluate simContext parent) (evaluate simContext mem)
        |> Result.defaultWith (fun _ -> JsonValue.Create(JsonValueKind.Null))

let evaluateIfNecessary simContext (rawStr: string) : JsonNode =
    if LanguageLexer.isLiteralStringWithAtSign rawStr then
        JsonValue.Create(rawStr.[1..])
    else if LanguageLexer.requiresInterpolation rawStr then
        rawStr |> LanguageLexer.lex |> LanguageParser.parse |> evaluate simContext
    else
        JsonValue.Create(rawStr)
