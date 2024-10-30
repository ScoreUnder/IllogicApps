module IllogicApps.Simulator.Test.TestSimUtil

open System.Text.Json.Nodes
open NUnit.Framework
open Swensen.Unquote
open IllogicApps.Core
open IllogicApps.Simulator
open JsonUtil

let makeSimulator () = Foq.Mock<SimulatorContext>().Create()

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

let rec stringOfAst =
    function
    | LanguageParser.Literal(lit) -> lit.ToJsonString(sensibleSerialiserOptions)
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
        | LanguageParser.Call(name, args) ->
            args
            |> List.map trace'
            |> TraceResult.sequence
            |> TraceResult.step (fun args -> LanguageParser.Call(name, args)) (fun args ->
                match BuiltinFunctions.functions.TryGetValue(name) with
                | true, func -> Changes(args |> List.map unpackLiteral |> func simContext |> LanguageParser.Literal)
                | _ -> TraceError $"Function {name} not found")
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
                | LanguageEvaluator.ForgivableError _ -> Changes(LanguageParser.Literal(jsonNull))
                | LanguageEvaluator.SeriousError err -> TraceError err)
        | LanguageParser.BuiltinConcat(args) ->
            args
            |> List.map trace'
            |> TraceResult.sequence
            |> TraceResult.step LanguageParser.BuiltinConcat (fun args ->
                args
                |> List.map (unpackLiteral >> BuiltinFunctions.objectToString)
                |> String.concat ""
                |> JsonValue.Create
                :> JsonNode
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
        | TraceError err -> Error err :: acc

    trace'' [ Ok expr ] expr |> List.rev

let traceEvaluation expr =
    if LanguageLexer.isLiteralStringWithAtSign expr then
        [ JsonValue.Create(expr.[1..]).ToJsonString(sensibleSerialiserOptions) ]
    else if LanguageLexer.requiresInterpolation expr then
        expr
        |> LanguageLexer.lex
        |> LanguageParser.parse
        |> traceEvaluationParsed
        |> List.map stringOfAstResult
    else
        [ JsonValue.Create(expr).ToJsonString(sensibleSerialiserOptions) ]

let traceEvaluationTo f expr = traceEvaluation expr |> List.iter f

let traceEvaluationParsedTo f expr =
    traceEvaluationParsed expr |> Seq.map stringOfAstResult |> Seq.iter f

let testOrTrace expr quote =
    try
        test quote
    with e ->
        traceEvaluationTo System.Console.WriteLine expr
        reraise ()

let raisesOrTrace<'e when 'e :> exn> expr quote =
    try
        raises<'e> quote
    with
    | :? AssertionException
    | :? AssertionFailedException ->
        traceEvaluationTo System.Console.WriteLine expr
        reraise ()

let testOrTraceParsed expr quote =
    try
        test quote
    with e ->
        traceEvaluationParsedTo System.Console.WriteLine expr
        reraise ()


let raisesOrTraceParsed<'e when 'e :> exn> expr quote =
    try
        raises<'e> quote
    with
    | :? AssertionException
    | :? AssertionFailedException ->
        traceEvaluationParsedTo System.Console.WriteLine expr
        reraise ()

let raisesWithOrTraceParsed<'e when 'e :> exn> expr quote f =
    try
        raisesWith<'e> quote f
    with
    | :? AssertionException
    | :? AssertionFailedException ->
        traceEvaluationParsedTo System.Console.WriteLine expr
        reraise ()
