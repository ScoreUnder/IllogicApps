module IllogicApps.Simulator.LanguageEvaluator

open System.Text.Json.Nodes
open System.Text.Json

type MemberAccessResult =
    | AccessOk of JsonNode
    | ForgivableError of string
    | SeriousError of string

    static member get(result: MemberAccessResult) =
        match result with
        | AccessOk value -> value
        | ForgivableError err -> failwith err
        | SeriousError err -> failwith err

    static member defaultWith (f: string -> JsonNode) (result: MemberAccessResult) =
        match result with
        | AccessOk value -> value
        | ForgivableError err -> f err
        | SeriousError err -> failwith err

let accessMember (parent: JsonNode) (mem: JsonNode) =
    if parent = null then
        ForgivableError "Cannot access property of null"
    else
        match parent.GetValueKind(), mem.GetValueKind() with
        | JsonValueKind.Object, JsonValueKind.String ->
            let propName = mem.GetValue<string>()

            parent.AsObject().TryGetPropertyValue(propName)
            |> function
                | true, value -> AccessOk value
                | _ -> ForgivableError(sprintf "Property %s not found" propName)
        | JsonValueKind.Object, kind -> SeriousError(sprintf "Cannot access property of object using %A" kind)
        | JsonValueKind.Array, JsonValueKind.Number ->
            let index = mem.GetValue<int>()
            let arr = parent.AsArray()

            if index >= 0 && index < arr.Count then
                AccessOk(arr.[index])
            else
                ForgivableError(sprintf "Index %d out of bounds in array of length %d" index arr.Count)
        | JsonValueKind.Array, kind -> SeriousError(sprintf "Cannot index array with %A" kind)
        | JsonValueKind.Null, _ -> ForgivableError "Cannot access property of null"
        | kind, _ -> SeriousError(sprintf "Cannot access property of %A" kind)

let rec evaluate simContext (ast: LanguageParser.Ast) =
    match ast with
    | LanguageParser.Literal(lit) -> lit
    | LanguageParser.Call(name, args) ->
        match BuiltinFunctions.functions.TryGetValue(name) with
        | true, func -> args |> List.map (evaluate simContext) |> func simContext
        | _ -> failwithf "Function %s not found" name
    | LanguageParser.Member(parent, mem) ->
        accessMember (evaluate simContext parent) (evaluate simContext mem)
        |> MemberAccessResult.get
    | LanguageParser.ForgivingMember(parent, mem) ->
        accessMember (evaluate simContext parent) (evaluate simContext mem)
        |> MemberAccessResult.defaultWith (fun _ -> JsonValue.Create(JsonValueKind.Null))
    | LanguageParser.BuiltinConcat(args) ->
        args
        |> List.toSeq
        |> Seq.map (evaluate simContext)
        |> Seq.map BuiltinFunctions.objectToString
        |> String.concat ""
        |> JsonValue.Create
        :> JsonNode

let evaluateIfNecessary simContext (rawStr: string) : JsonNode =
    if LanguageLexer.isLiteralStringWithAtSign rawStr then
        JsonValue.Create(rawStr.[1..])
    else if LanguageLexer.requiresInterpolation rawStr then
        rawStr |> LanguageLexer.lex |> LanguageParser.parse |> evaluate simContext
    else
        JsonValue.Create(rawStr)
