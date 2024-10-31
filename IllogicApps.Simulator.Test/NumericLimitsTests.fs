module IllogicApps.Simulator.Test.NumericLimitsTests

open System
open NUnit.Framework
open Swensen.Unquote

open IllogicApps.Core.JsonUtil
open TestSimUtil

[<TestCase("@{18446744073709551615}")>]
[<TestCase("@{9223372036854775808}")>]
[<TestCase("@{-9223372036854775809}")>]
let UnlexableOutOfRangeTest (expr: string) =
    raises<OverflowException> <@ lexExpr expr @>

[<TestCase("@{9223372036854775807}", "9223372036854775807")>]
[<TestCase("@{-9223372036854775808}", "-9223372036854775808")>]
[<TestCase("@{9223372036854775809.5}", "9.223372036854776E+18")>]
[<TestCase("@{-9223372036854775809.5}", "-9.223372036854776E+18")>]
let ExecInRangeTest (expr: string) (expected: string) =
    testOrTrace expr <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>

[<TestCase("@int('9223372036854775808')")>]
[<TestCase("@int('9223372036854775807')")>]
[<TestCase("@decimal('3e-50')")>]
[<TestCase("@decimal('3e50')")>]
[<TestCase("@int('3e50')")>]
[<TestCase("@int(mul(2e200,2e200))")>]
let ExecConvertOutOfRangeTest (expr: string) =
    let parsed = trap <@ parseExpr (lexExpr expr) @>
    raisesWithOrTraceParsed<Exception> parsed <@ evaluateParsed parsed |> ignore @> <| fun ex ->
        <@ ex.Message.Contains("Could not parse") || ex :? OverflowException @>

[<TestCase("@int('3e-50')")>]
[<TestCase("@int(1.5)")>]
[<TestCase("@int('1.5')")>]
[<TestCase("@int(2.5)")>]
[<TestCase("@int(100000.000009)")>]
let ExecNoFractionalIntegersTest (expr: string) =
    let parsed = trap <@ parseExpr (lexExpr expr) @>
    raisesWithOrTraceParsed<Exception> parsed <@ evaluateParsed parsed |> ignore @> <| fun ex ->
        <@ ex.Message.Contains("Could not parse") || ex :? OverflowException @>

[<TestCase("@{int('-9223372036854775808')}", "-9223372036854775808")>]
[<TestCase("@{int('-9223372036854775809')}", "-9223372036854775808")>] // Fucking incredible
[<TestCase("@{int('-9223372036854775809.00')}", "-9223372036854775808")>]
[<TestCase("@{int('-9223372036854775809.05')}", "-9223372036854775808")>]
[<TestCase("@{float('5e-324')}", "5E-324")>]
[<TestCase("@{float('1.7e+308')}", "1.7E+308")>]
[<TestCase("@{float('5e-350')}", "0")>]
[<TestCase("@{float('1.7e+350')}", "Infinity")>]
[<TestCase("@{decimal(100000.000009)}", "100000.000009")>]
[<TestCase("@{decimal('10000000000.00000000001234')}", "10000000000.00000000001234")>]
let ExecConvertInRangeTest (expr: string) (expected: string) =
    testOrTrace expr <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>

[<TestCase("@{mul(decimal('7e28'), 10)}")>]
[<TestCase("@{add(9223372036854775807, 1)}")>]
[<TestCase("@{mul(4294967295, 4294967295)}")>]
[<TestCase("@{mul(max(20, decimal('10'), 30.1), 1e50)}")>]  // Converts 30.1 up to decimal
[<TestCase("@div(-1, 0)")>]
[<TestCase("@div(0, 0)")>]
let ExecMathOutOfRangeTest (expr: string) =
    let parsed = trap <@ parseExpr (lexExpr expr) @>
    raisesOrTraceParsed<ArithmeticException> parsed <@ evaluateParsed parsed |> ignore @>

[<TestCase("@{add(2147483647, 1)}", "2147483648")>]
[<TestCase("@{add(4294967295, 1)}", "4294967296")>]
[<TestCase("@{mul(1e308, 2)}", "Infinity")>]
[<TestCase("@{float(mul(2e200,2e200))}", "Infinity")>]
[<TestCase("@{mul(4294967295, 4294967295.0)}", "1.8446744065119617E+19")>]
[<TestCase("@{mul(decimal('4294967295'), 4294967295)}", "18446744065119617025")>]
[<TestCase("@{mul(decimal('4294967295'), 4294967295.0)}", "18446744065119617025")>]
let ExecMathInRangeTest (expr: string) (expected: string) =
    testOrTrace expr <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>
