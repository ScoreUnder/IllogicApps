module IllogicApps.Simulator.Test.TestSimUtil

open IllogicApps.Json
open Swensen.Unquote
open IllogicApps.Core
open IllogicApps.Simulator

let makeSimulator () =
    Foq.Mock<SimulatorContext>()
        .Setup(fun x -> <@ x.IsBugForBugAccurate @>).Returns(true)
        .Create()

let testExpressionEvaluation expr =
    LanguageEvaluator.evaluateIfNecessary (makeSimulator ()) expr

let lexExpr expr =
    test
        <@
            not (LanguageLexer.isLiteralStringWithAtSign expr)
            && LanguageLexer.requiresInterpolation expr
        @>

    LanguageLexer.lex expr

let parseExpr lexed = LanguageParser.parse lexed

let evaluateParsed expr =
    LanguageEvaluator.evaluate (makeSimulator ()) expr

type private 'a TraceResult =
    | NoChanges of 'a
    | Changes of 'a
    | TraceError of string

    static member step (stepPause: 'a -> 'b) (stepOnce: 'a -> 'b TraceResult) (a: 'a TraceResult) =
        match a with
        | NoChanges a -> stepOnce a
        | Changes a -> Changes(stepPause a)
        | TraceError err -> TraceError err

    static member get(a: 'a TraceResult) =
        match a with
        | NoChanges a -> a
        | Changes a -> a
        | TraceError err -> failwith err

    static member map2 (f: 'a -> 'b -> 'c) (a: 'a TraceResult) (b: 'b TraceResult) =
        match a, b with
        | TraceError err, _ -> TraceError err
        | _, TraceError err -> TraceError err
        | NoChanges a, NoChanges b -> NoChanges(f a b)
        | _ -> Changes(f (TraceResult.get a) (TraceResult.get b))

    static member sequence(a: 'a TraceResult seq) : 'a list TraceResult =
        Seq.foldBack (TraceResult.map2 (fun a acc -> a :: acc)) a (NoChanges [])

    static member tuple2 (a: 'a TraceResult) (b: 'b TraceResult) =
        TraceResult.map2 (fun a b -> (a, b)) a b

type private NeedEvaluationException(index, parent, ast) =
    inherit System.Exception()

    member this.Index = index
    member this.Parent = parent
    member this.Ast = ast

let rec stringOfAst =
    function
    | LanguageParser.Literal(lit) -> Conversions.stringOfJson lit
    | LanguageParser.Call(name, args) -> $"""{name}({args |> List.map stringOfAst |> String.concat ", "})"""
    | LanguageParser.Member(parent, mem) -> $"{stringOfAst parent}[{stringOfAst mem}]"
    | LanguageParser.ForgivingMember(parent, mem) -> $"{stringOfAst parent}?[{stringOfAst mem}]"
    | LanguageParser.BuiltinConcat(args) -> $"""concat({args |> List.map stringOfAst |> String.concat ", "})"""

let stringOfAstResult =
    function
    | Ok v -> stringOfAst v
    | Error err -> $"Error: {err}"

let traceEvaluationParsed expr =
    let unpackLiteral =
        function
        | LanguageParser.Literal(lit) -> lit
        | _ -> failwith "Expected a literal"

    let simContext = makeSimulator ()

    let rec trace' =
        function
        | LanguageParser.Literal _ as ast -> NoChanges ast
        | LanguageParser.Call(name, args) as ast ->
            match OrderedMap.tryFindCaseInsensitive name BuiltinFunctions.functions with
            | Some func ->
                args
                |> List.map trace'
                |> TraceResult.sequence
                |> TraceResult.step (fun args -> LanguageParser.Call(name, args)) (fun args ->
                    match BuiltinFunctions.functions.TryGetValue(name) with
                    | true, func ->
                        Changes(args |> List.map unpackLiteral |> func simContext |> LanguageParser.Literal)
                    | _ -> TraceError <| LanguageEvaluator.ErrorMessages.badFunctionCall name)
            | _ ->
                match OrderedMap.tryFindCaseInsensitive name BuiltinFunctions.lazyFunctions with
                | Some func ->
                    try
                        args
                        |> List.mapi (fun i v ->
                            lazy
                                match v with
                                | LanguageParser.Literal v -> v
                                | _ -> raise (NeedEvaluationException(i, ast, v)))
                        |> func simContext
                        |> LanguageParser.Literal
                        |> Changes
                    with :? NeedEvaluationException as e ->
                        let i = e.Index
                        let parent = e.Parent

                        e.Ast
                        |> trace'
                        |> TraceResult.step
                            (fun evaluated ->
                                let newArgs = args |> List.mapi (fun j v -> if i = j then evaluated else v)
                                LanguageParser.Call(name, newArgs))
                            (fun _ ->
                                TraceError
                                    $"Internal error: Expected evaluation of argument {i} of {parent} to change, but it didn't")
                | _ -> TraceError <| LanguageEvaluator.ErrorMessages.badFunctionCall name
        | LanguageParser.Member(parent, mem) ->
            TraceResult.tuple2 (trace' parent) (trace' mem)
            |> TraceResult.step LanguageParser.Member (fun (parent, mem) ->
                match LanguageEvaluator.accessMember (unpackLiteral parent) (unpackLiteral mem) with
                | LanguageEvaluator.AccessOk value -> Changes(LanguageParser.Literal(value))
                | LanguageEvaluator.ForgivableError err -> TraceError err
                | LanguageEvaluator.SeriousError err -> TraceError err)
        | LanguageParser.ForgivingMember(parent, mem) ->
            TraceResult.tuple2 (trace' parent) (trace' mem)
            |> TraceResult.step LanguageParser.ForgivingMember (fun (parent, mem) ->
                match LanguageEvaluator.accessMember (unpackLiteral parent) (unpackLiteral mem) with
                | LanguageEvaluator.AccessOk value -> Changes(LanguageParser.Literal(value))
                | LanguageEvaluator.ForgivableError _ -> Changes(LanguageParser.Literal(Null))
                | LanguageEvaluator.SeriousError err -> TraceError err)
        | LanguageParser.BuiltinConcat(args) ->
            args
            |> List.map trace'
            |> TraceResult.sequence
            |> TraceResult.step LanguageParser.BuiltinConcat (fun args ->
                args
                |> List.map (unpackLiteral >> BuiltinFunctions.objectToString)
                |> String.concat ""
                |> String
                |> LanguageParser.Literal
                |> Changes)

    let rec trace'' acc (ast: LanguageParser.Ast) =
        let result =
            try
                trace' ast
            with e ->
                TraceError $"{e.GetType().Name}: {e.Message}"

        match result with
        | NoChanges ast -> Ok ast :: acc
        | Changes nextAst -> trace'' (Ok ast :: acc) nextAst
        | TraceError err -> Error err :: Ok ast :: acc

    trace'' [] expr |> List.rev

let traceEvaluation expr =
    if LanguageLexer.isLiteralStringWithAtSign expr then
        [ String expr.[1..] |> Conversions.stringOfJson ]
    else if LanguageLexer.requiresInterpolation expr then
        expr
        |> LanguageLexer.lex
        |> LanguageParser.parse
        |> traceEvaluationParsed
        |> List.map stringOfAstResult
    else
        [ String expr |> Conversions.stringOfJson ]

let traceEvaluationTo f expr = traceEvaluation expr |> List.iter f

let traceEvaluationParsedTo f expr =
    traceEvaluationParsed expr |> Seq.map stringOfAstResult |> Seq.iter f

let testOrTrace expr quote =
    try
        test quote
    with _ ->
        traceEvaluationTo System.Console.WriteLine expr
        reraise ()

let raisesOrTrace<'e when 'e :> exn> expr quote =
    try
        raises<'e> quote
    with _ ->
        traceEvaluationTo System.Console.WriteLine expr
        reraise ()

let raisesWithOrTrace<'e when 'e :> exn> expr quote f =
    try
        raisesWith<'e> quote f
    with _ ->
        traceEvaluationTo System.Console.WriteLine expr
        reraise ()

let testOrTraceParsed expr quote =
    try
        test quote
    with _ ->
        traceEvaluationParsedTo System.Console.WriteLine expr
        reraise ()


let raisesOrTraceParsed<'e when 'e :> exn> expr quote =
    try
        raises<'e> quote
    with _ ->
        traceEvaluationParsedTo System.Console.WriteLine expr
        reraise ()

let raisesWithOrTraceParsed<'e when 'e :> exn> expr quote f =
    try
        raisesWith<'e> quote f
    with _ ->
        traceEvaluationParsedTo System.Console.WriteLine expr
        reraise ()

let stringTest expr expected =
    testOrTrace expr <@ String expected = testExpressionEvaluation expr @>

let objTest expr expected =
    testOrTrace expr <@ Parser.parse expected = testExpressionEvaluation expr @>

let jsonTest (expr: string) (expected: JsonTree) =
    testOrTrace expr <@ expected = testExpressionEvaluation expr @>

let stringOrFailTest (expr: string) (expected: Result<string, string>) =
    match expected with
    | Ok expected -> testOrTrace expr <@ String expected = testExpressionEvaluation expr @>
    | Error expected ->
        raisesWithOrTrace<System.Exception> expr <@ testExpressionEvaluation expr @> (fun e ->
            let message = e.Message in <@ message.Contains(expected) @>)

let boolOrFailTest (expr: string) (expected: Result<bool, string>) =
    match expected with
    | Ok expected -> testOrTrace expr <@ Boolean expected = testExpressionEvaluation expr @>
    | Error expected ->
        raisesWithOrTrace<System.Exception> expr <@ testExpressionEvaluation expr @> (fun e ->
            let message = e.Message in <@ message.Contains(expected) @>)

let objOrFailTest (expr: string) (expected: Result<string, string>) =
    match expected with
    | Ok expected -> testOrTrace expr <@ Parser.parse expected = testExpressionEvaluation expr @>
    | Error expected ->
        raisesWithOrTrace<System.Exception> expr <@ testExpressionEvaluation expr @> (fun e ->
            let message = e.Message in <@ message.Contains(expected) @>)

let jsonOrFailTest (expr: string) (expected: Result<JsonTree, string>) =
    match expected with
    | Ok expected -> testOrTrace expr <@ expected = testExpressionEvaluation expr @>
    | Error expected ->
        raisesWithOrTrace<System.Exception> expr <@ testExpressionEvaluation expr @> (fun e ->
            let message = e.Message in <@ message.Contains(expected) @>)
