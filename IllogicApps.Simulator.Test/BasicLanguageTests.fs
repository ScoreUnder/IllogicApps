module IllogicApps.Simulator.Test.BasicLanguageTests

open System
open NUnit.Framework
open Swensen.Unquote

open IllogicApps.Json
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
    test <@ String str = testExpressionEvaluation str @>

[<TestCase("@@unparsed!")>]
[<TestCase("@@{still unparsed}")>]
[<TestCase("@@,,}")>]
[<TestCase("@@{'abc'}@{'def'}@@{'ghi'}j@{'kl'}")>]
let StringAtSignPassthroughTest (str: string) =
    test <@ String str.[1..] = testExpressionEvaluation str @>

[<TestCase("@null")>]
[<TestCase("@true")>]
[<TestCase("@false")>]
let LiteralParsingTest (expr: string) =
    test <@ Parser.parse expr.[1..] = testExpressionEvaluation expr @>

[<TestCase("@1", 1L)>]
[<TestCase("@-51", -51L)>]
[<TestCase("@+54", 54L)>]
let NumericIntegerParsingTest (expr: string) (expected: int64) =
    test <@ Integer expected = testExpressionEvaluation expr @>

[<TestCase("@0.5", 0.5)>]
[<TestCase("@+.5", 0.5)>]
[<TestCase("@+1.", 1.0)>]
[<TestCase("@+15.75", 15.75)>]
[<TestCase("@-01.0", -1.0)>]
[<TestCase("@3e1", 30.0)>]
[<TestCase("@3.2e1", 32.0)>]
[<TestCase("@0.9e+1", 9.0)>]
[<TestCase("@3.2e-1", 0.32)>]
let NumericDoubleParsingTest (expr: string) (expected: float) =
    test <@ Float expected = testExpressionEvaluation expr @>


[<TestCase("@{1}", "1")>]
[<TestCase("@{-51}", "-51")>]
[<TestCase("@{+54}", "54")>]
[<TestCase("@{3e1}", "30")>]
[<TestCase("@{+1.}", "1")>]
let NumericIntegerInterpolationTest (expr: string) (expected: string) = stringTest expr expected

[<TestCase("@{0.5}", "0.5")>]
[<TestCase("@{+.5}", "0.5")>]
[<TestCase("@{+15.75}", "15.75")>]
[<TestCase("@{-01.0}", "-1")>]
[<TestCase("@{3.2e1}", "32")>]
[<TestCase("@{0.9e+1}", "9")>]
[<TestCase("@{3.2e-1}", "0.32")>]
let NumericDoubleInterpolationTest (expr: string) (expected: string) = stringTest expr expected

[<TestCase("@'test'", "test")>]
[<TestCase("@'don''t worry'", "don't worry")>]
let StringParsingTest (expr: string) (expected: string) = stringTest expr expected

[<TestCase("@{'test'}", "test")>]
[<TestCase("@{'don''t worry'}", "don't worry")>]
[<TestCase("foo @{'bar'} baz", "foo bar baz")>]
[<TestCase("@{'h'}@{'e'}@{'l'}@{'l'}@{'o'}", "hello")>]
[<TestCase("@{'hello'} @{'world'}", "hello world")>]
let InterpolatedStringParsingTest (expr: string) (expected: string) = stringTest expr expected

[<TestCase("testing @@{'blah'} @{'thing'}", "testing @{'blah'} thing")>]
let InterpolatedStringWithPassthroughTest (expr: string) (expected: string) = stringTest expr expected

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
let ConstantsInterpolationTest (expr: string) (expected: string) = stringTest expr expected

let ``Case insensitive access test cases`` =
    [ "@json('{\"abCDef\": 123}').abcdef", Integer 123L
      "@json('{\"abCDef\": 123}')?.abcdef", Integer 123L
      "@json('{\"abCDef\": 123}').AbcdEF", Integer 123L
      "@json('{\"abCDef\": 123}')['abcdef']", Integer 123L
      "@json('{\"abCDef\": 123}')?['abcdef']", Integer 123L
      "@json('{\"abCDef\": 123}')['AbcdEF']", Integer 123L
      "@json('{\"a\": 1, \"a\": 2}').a", Integer 2L
      "@json('{\"a\": 1, \"A\": 2}').a", Integer 1L
      "@json('{\"A\": 1, \"a\": 2}').a", Integer 2L
      "@json('{\"A\": 1, \"A\": 2}').a", Integer 2L
      "@json('{\"a\": 1, \"a\": 2}').A", Integer 2L
      "@json('{\"a\": 1, \"A\": 2}').A", Integer 2L
      "@json('{\"A\": 1, \"a\": 2}').A", Integer 1L
      "@json('{\"A\": 1, \"A\": 2}').A", Integer 2L
      "@json('{\"aAa\": 1, \"AaA\": 2}').aaa", Integer 1L
      "@json('{\"AaA\": 1, \"aAa\": 2}').aaa", Integer 1L
      "@json('{\"aAa\": 1, \"AaA\": 2}').AAA", Integer 1L
      "@json('{\"AaA\": 1, \"aAa\": 2}').AAA", Integer 1L ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``Case insensitive access test cases``)>]
let ``Test case insensitive member access`` expr expected = jsonTest expr expected

let ``Case-conflicting JSON parsing test cases`` =
    [ "@json('{\"a\": 1, \"a\": 2}')", Conversions.createObject [ "a", Integer 2L ]
      "@json('{\"a\": 1, \"A\": 2}')", Conversions.createObject [ "a", Integer 1L; "A", Integer 2L ]
      "@json('{\"A\": 1, \"a\": 2}')", Conversions.createObject [ "A", Integer 1L; "a", Integer 2L ]
      "@json('{\"A\": 1, \"A\": 2}')", Conversions.createObject [ "A", Integer 2L ]
      "@json('{\"a\": 1, \"b\": -1, \"a\": 2}')", Conversions.createObject [ "b", Integer -1L; "a", Integer 2L ] ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``Case-conflicting JSON parsing test cases``)>]
let ``Test case-conflicting JSON parsing`` expr expected = jsonTest expr expected
