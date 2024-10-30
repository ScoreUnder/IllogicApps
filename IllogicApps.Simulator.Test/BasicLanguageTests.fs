module IllogicApps.Simulator.Test.BasicLanguageTests

open System
open System.Text.Json
open System.Text.Json.Nodes
open NUnit.Framework
open Swensen.Unquote

open IllogicApps.Core.JsonUtil
open TestSimUtil

[<TestCase("str")>]
[<TestCase("1")>]
[<TestCase("null")>]
[<TestCase("[]")>]
[<TestCase("{}")>]
[<TestCase("")>]
[<TestCase(" @@,,}")>]
[<TestCase(" @,,}")>]
[<TestCase("@")>]
let StringPassthroughTest (str: string) =
    test <@ jsonsEqual (jsonOf str) (testExpressionEvaluation str) @>

[<TestCase("@@unparsed!")>]
[<TestCase("@@{still unparsed}")>]
[<TestCase("@@,,}")>]
let StringAtSignPassthroughTest (str: string) =
    test <@ jsonsEqual (jsonOf str.[1..]) (testExpressionEvaluation str) @>

[<TestCase("@null")>]
[<TestCase("@true")>]
[<TestCase("@false")>]
let LiteralParsingTest (expr: string) =
    let expected =
        match JsonSerializer.Deserialize<JsonNode>(expr.[1..]) with
        | null -> jsonNull
        | v -> v

    test <@ jsonsEqual expected (testExpressionEvaluation expr) @>

[<TestCase("@1", 1L)>]
[<TestCase("@-51", -51L)>]
[<TestCase("@+54", 54L)>]
[<TestCase("@3e1", 30L)>]
[<TestCase("@+1.", 1L)>]
let NumericIntegerParsingTest (expr: string) (expected: int64) =
    test <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>

[<TestCase("@0.5", 0.5)>]
[<TestCase("@+.5", 0.5)>]
[<TestCase("@+15.75", 15.75)>]
[<TestCase("@-01.0", -1.0)>]
[<TestCase("@3.2e1", 32.0)>]
[<TestCase("@0.9e+1", 9.0)>]
[<TestCase("@3.2e-1", 0.32)>]
let NumericDoubleParsingTest (expr: string) (expected: float) =
    test <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>


[<TestCase("@{1}", "1")>]
[<TestCase("@{-51}", "-51")>]
[<TestCase("@{+54}", "54")>]
[<TestCase("@{3e1}", "30")>]
[<TestCase("@{+1.}", "1")>]
let NumericIntegerInterpolationTest (expr: string) (expected: string) =
    test <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>

[<TestCase("@{0.5}", "0.5")>]
[<TestCase("@{+.5}", "0.5")>]
[<TestCase("@{+15.75}", "15.75")>]
[<TestCase("@{-01.0}", "-1")>]
[<TestCase("@{3.2e1}", "32")>]
[<TestCase("@{0.9e+1}", "9")>]
[<TestCase("@{3.2e-1}", "0.32")>]
let NumericDoubleInterpolationTest (expr: string) (expected: string) =
    test <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>

[<TestCase("@'test'", "test")>]
[<TestCase("@'don''t worry'", "don't worry")>]
let StringParsingTest (expr: string) (expected: string) =
    test <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>

[<TestCase("@{'test'}", "test")>]
[<TestCase("@{'don''t worry'}", "don't worry")>]
[<TestCase("foo @{'bar'} baz", "foo bar baz")>]
[<TestCase("@{'h'}@{'e'}@{'l'}@{'l'}@{'o'}", "hello")>]
[<TestCase("@{'hello'} @{'world'}", "hello world")>]
let InterpolatedStringParsingTest (expr: string) (expected: string) =
    test <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>

[<TestCase("@'a','b','c'")>]
[<TestCase("@{'a','b','c'}")>]
[<TestCase("@'a'}")>]
[<TestCase("@{'a'")>]
[<TestCase(" @{")>]
[<TestCase("@()")>]
[<TestCase("@{()}")>]
[<TestCase("@{}")>]
[<TestCase("@concat")>]
[<TestCase("@{concat}")>]
[<TestCase("@concat('a','b')('c')")>]
[<TestCase("@{concat('a','b')('c')}")>]
[<TestCase("@+")>]
[<TestCase("@{+}")>]
[<TestCase("@+1e")>]
[<TestCase("@{+1e}")>]
[<TestCase("@1e")>]
[<TestCase("@{1e}")>]
[<TestCase("@1e+")>]
[<TestCase("@{1e+}")>]
[<TestCase("@1e1.1")>]
[<TestCase("@{1e1.1}")>]
[<TestCase("@++1")>]
[<TestCase("@{++1}")>]
[<TestCase("@--1")>]
[<TestCase("@{--1}")>]
[<TestCase("@string( { \"name\": \"Sophie Owen\" } )")>]
let UnparseableTest (expr: string) =
    raises<Exception> <@ parseExpr (lexExpr expr) @>

[<TestCase("@{true}", "True")>]
[<TestCase("@{false}", "False")>]
[<TestCase("@{null}", "")>]
let ConstantsInterpolationTest (expr: string) (expected: string) =
    test <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>
