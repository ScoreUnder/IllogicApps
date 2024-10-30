module IllogicApps.Simulator.Test.NumericConversionTests

open System
open NUnit.Framework
open IllogicApps.Core.JsonUtil
open TestSimUtil

[<TestCase("@{float(decimal('10000000000.00000000001234'))}", "10000000000")>]
[<TestCase("@{float('  +1,000 ')}", "1000")>]
[<TestCase("@{float('  200- ')}", "-200")>]
let ExecConvertToFloatTest (expr: string) (expected: string) =
    testOrTrace expr <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>

[<TestCase("@{decimal('  +1,000 ')}", "1000")>]
[<TestCase("@{decimal('  200- ')}", "-200")>]
let ExecConvertToDecimalTest (expr: string) (expected: string) =
    testOrTrace expr <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>

[<TestCase("@{int(1.0)}", "1")>]
[<TestCase("@{int(float(1.0))}", "1")>]
[<TestCase("@{int(decimal('1.00000'))}", "1")>]
[<TestCase("@{int(string(decimal(100000.00000)))}", "100000")>]
[<TestCase("@{int('  +1,000 ')}", "1000")>]
let ExecConvertToIntTest (expr: string) (expected: string) =
    testOrTrace expr <@ jsonsEqual (jsonOf expected) (testExpressionEvaluation expr) @>

[<TestCase("@{int('  200- ')}", "-200")>]
let ExecInvalidConvertToIntTest (expr: string) =
    raisesOrTrace<Exception> expr <@ testExpressionEvaluation expr @>
