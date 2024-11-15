module IllogicApps.Simulator.LanguageEvaluator

open IllogicApps.Core
open IllogicApps.Json

module ErrorMessages =
    let badFunctionCall name =
        $"The template function '{name}' is not defined or not valid"

type MemberAccessResult =
    | AccessOk of JsonTree
    | ForgivableError of string
    | SeriousError of string

    static member get(result: MemberAccessResult) =
        match result with
        | AccessOk value -> value
        | ForgivableError err -> failwith err
        | SeriousError err -> failwith err

    static member defaultWith (f: string -> JsonTree) (result: MemberAccessResult) =
        match result with
        | AccessOk value -> value
        | ForgivableError err -> f err
        | SeriousError err -> failwith err

let accessMember (parent: JsonTree) (mem: JsonTree) =
    match parent, mem with
    | Object parent, String propName ->
        match OrderedMap.tryFindCaseInsensitive propName parent with
        | Some value -> AccessOk value
        | _ -> ForgivableError(sprintf "Property %s not found" propName)
    | Object _, other -> SeriousError(sprintf "Cannot access property of object using %A" (JsonTree.getType other))
    | Array arr, Integer index ->
        if index >= 0 && index < arr.Length then
            AccessOk(arr.[int index])
        else
            ForgivableError(sprintf "Index %d out of bounds in array of length %d" index arr.Length)
    | Array _, other -> SeriousError(sprintf "Cannot index array with %A" (JsonTree.getType other))
    | Null, _ -> ForgivableError "Cannot access property of null"
    | kind, _ -> SeriousError(sprintf "Cannot access property of %A" kind)

type FuncsMap = Map<string, BuiltinFunctions.LanguageFunction>
type LazyFuncsMap = Map<string, BuiltinFunctions.LazyArgsLanguageFunction>

let evaluateSandboxed (functions: FuncsMap) (lazyFunctions: LazyFuncsMap) simContext (ast: LanguageParser.Ast) =
    let rec evaluate' simContext (ast: LanguageParser.Ast) =
        match ast with
        | LanguageParser.Literal(lit) -> lit
        | LanguageParser.Call(name, args) ->
            match functions.TryGetValue(name) with
            | true, func -> args |> List.map (evaluate' simContext) |> func simContext
            | _ ->
                match lazyFunctions.TryGetValue(name) with
                | true, func -> args |> List.map (fun ast -> lazy evaluate' simContext ast) |> func simContext
                | _ -> failwith <| ErrorMessages.badFunctionCall name
        | LanguageParser.Member(parent, mem) ->
            accessMember (evaluate' simContext parent) (evaluate' simContext mem)
            |> MemberAccessResult.get
        | LanguageParser.ForgivingMember(parent, mem) ->
            accessMember (evaluate' simContext parent) (evaluate' simContext mem)
            |> MemberAccessResult.defaultWith (fun _ -> Null)
        | LanguageParser.BuiltinConcat(args) ->
            args
            |> List.toSeq
            |> Seq.map (evaluate' simContext)
            |> Seq.map BuiltinFunctions.objectToString
            |> String.concat ""
            |> String

    evaluate' simContext ast

let inline evaluate simContext (ast: LanguageParser.Ast) =
    evaluateSandboxed BuiltinFunctions.functions BuiltinFunctions.lazyFunctions simContext ast

let altEvaluateIfNecessary altEvaluate (simContext: SimulatorContext) (rawStr: string) : JsonTree =
    if LanguageLexer.isLiteralStringWithAtSign rawStr then
        String(rawStr.[1..])
    else if LanguageLexer.requiresInterpolation rawStr then
        rawStr |> LanguageLexer.lex |> LanguageParser.parse |> altEvaluate simContext
    else
        String(rawStr)

let inline evaluateIfNecessary simContext rawStr =
    altEvaluateIfNecessary evaluate simContext rawStr
