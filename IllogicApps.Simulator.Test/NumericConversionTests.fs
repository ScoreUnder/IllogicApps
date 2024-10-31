module IllogicApps.Simulator.Test.NumericConversionTests

open System
open NUnit.Framework
open IllogicApps.Core.JsonUtil
open TestSimUtil

[<TestCase("@{float(decimal('10000000000.00000000001234'))}", "10000000000")>]
[<TestCase("@{float('  +1,000 ')}", "1000")>]
[<TestCase("@{float('  200- ')}", "-200")>]
[<TestCase("@{float('NaN')}", "NaN")>]
[<TestCase("@{float('nan')}", "NaN")>]
let ExecConvertToFloatTest (expr: string) (expected: string) =
    testOrTrace expr <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>

[<TestCase("@{decimal('  +1,000 ')}", "1000")>]
[<TestCase("@{decimal('  200- ')}", "-200")>]
let ExecConvertToDecimalTest (expr: string) (expected: string) =
    testOrTrace expr <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>

[<TestCase("@{int(1.0)}", "1")>]
[<TestCase("@{int('1.0')}", "1")>]
[<TestCase("@{int(float(1.0))}", "1")>]
[<TestCase("@{int(decimal('1.00000'))}", "1")>]
[<TestCase("@{int(string(decimal(100000.00000)))}", "100000")>]
[<TestCase("@{int('  +1,000 ')}", "1000")>]
let ExecConvertToIntTest (expr: string) (expected: string) =
    testOrTrace expr <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>

[<TestCase("@{int('  200- ')}")>]
[<TestCase("@{int('(200)')}")>]
let ExecInvalidConvertToIntTest (expr: string) =
    raisesOrTrace<Exception> expr <@ testExpressionEvaluation expr @>

[<TestCase("@{float('(200)')}")>]
[<TestCase("@{float('Infinity')}")>]
[<TestCase("@{float('inf')}")>]
let ExecInvalidConvertToFloatTest (expr: string) =
    raisesWithOrTrace<Exception> expr <@ testExpressionEvaluation expr @>
    <| fun e -> let message = e.Message in <@ message.Contains("Could not parse") @>

[<TestCase("@{decimal('(200)')}")>]
let ExecInvalidConvertToDecimalTest (expr: string) =
    raisesOrTrace<Exception> expr <@ testExpressionEvaluation expr @>

[<TestCase("@decimal(binary('123.0'))")>]
[<TestCase("@float(binary('123.0'))")>]
[<TestCase("@int(binary('123.0'))")>]
let ExecInvalidConvertFromBinaryTest (expr: string) =
    raisesOrTrace<Exception> expr <@ testExpressionEvaluation expr @>
