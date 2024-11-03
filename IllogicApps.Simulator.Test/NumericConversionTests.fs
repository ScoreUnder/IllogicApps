module IllogicApps.Simulator.Test.NumericConversionTests

open System
open NUnit.Framework

open IllogicApps.Json
open TestSimUtil

[<TestCase("@{float(decimal('10000000000.00000000001234'))}", "10000000000")>]
[<TestCase("@{float('  +1,000 ')}", "1000")>]
[<TestCase("@{float('  200- ')}", "-200")>]
[<TestCase("@{float('NaN')}", "NaN")>]
[<TestCase("@{float('nan')}", "NaN")>]
[<TestCase("@{float('Infinity','')}", "Infinity")>]
[<TestCase("@{float(float('Infinity',''))}", "Infinity")>]
[<TestCase("@{float('400,000','en-gb')}", "400000")>]
[<TestCase("@{float('400.000','en-gb')}", "400")>]
[<TestCase("@{float('400,000','de-de')}", "400")>]
[<TestCase("@{float('400.000','de-de')}", "400000")>]
let ExecConvertToFloatTest (expr: string) (expected: string) =
    testOrTrace expr <@ String expected = testExpressionEvaluation expr @>

[<TestCase("@{decimal('  +1,000 ')}", "1000")>]
[<TestCase("@{decimal('  200- ')}", "-200")>]
let ExecConvertToDecimalTest (expr: string) (expected: string) =
    testOrTrace expr <@ String expected = testExpressionEvaluation expr @>

[<TestCase("@{int(1.0)}", "1")>]
[<TestCase("@{int('1.0')}", "1")>]
[<TestCase("@{int(float(1.0))}", "1")>]
[<TestCase("@{int(decimal('1.00000'))}", "1")>]
[<TestCase("@{int(string(decimal(100000.00000)))}", "100000")>]
[<TestCase("@{int('  +1,000 ')}", "1000")>]
[<TestCase("@{int('nan')}", "-9223372036854775808")>] // Another incredible case from Logic Apps
[<TestCase("@{int('100000000000000010')}", "100000000000000016")>]
let ExecConvertToIntTest (expr: string) (expected: string) =
    testOrTrace expr <@ String expected = testExpressionEvaluation expr @>

[<TestCase("@{int('  200- ')}")>]
[<TestCase("@{int('(200)')}")>]
let ExecInvalidConvertToIntTest (expr: string) =
    raisesOrTrace<Exception> expr <@ testExpressionEvaluation expr @>

[<TestCase("@{float('(200)')}")>]
[<TestCase("@{float('Infinity')}")>]
[<TestCase("@{float('inf')}")>]
[<TestCase("@{float(string(float('Infinity','')))}")>]
let ExecInvalidConvertToFloatTest (expr: string) =
    raisesWithOrTrace<Exception> expr <@ testExpressionEvaluation expr @>
    <| fun e -> let message = e.Message in <@ message.Contains("Could not parse") @>

[<TestCase("@{decimal('(200)')}")>]
[<TestCase("@{decimal('3e-1')}")>]
[<TestCase("@{decimal('7e28')}")>]
let ExecInvalidConvertToDecimalTest (expr: string) =
    raisesOrTrace<Exception> expr <@ testExpressionEvaluation expr @>

[<TestCase("@decimal(binary('123.0'))")>]
[<TestCase("@float(binary('123.0'))")>]
[<TestCase("@int(binary('123.0'))")>]
let ExecInvalidConvertFromBinaryTest (expr: string) =
    raisesOrTrace<Exception> expr <@ testExpressionEvaluation expr @>

// Note: isFloat does not test if the value passed is a float,
// it checks if it is a string which is convertible to a float
[<TestCase("@{isFloat(json('{\"x\":\"Infinity\"}').x)}", "True")>] // (Proves that "Infinity" is not parsed as float implicitly already)
[<TestCase("@{isFloat('NaN')}", "True")>]
[<TestCase("@{isFloat('NaN','')}", "True")>]
[<TestCase("@{isFloat('NaN','en-gb')}", "True")>]
[<TestCase("@{isFloat('NaN','en-us')}", "True")>]
[<TestCase("@{isFloat('NaN','de-de')}", "True")>]
[<TestCase("@{isFloat('Infinity')}", "True")>]
[<TestCase("@{isFloat('Infinity','')}", "True")>]
[<TestCase("@{isFloat('infINITY','')}", "True")>]
[<TestCase("@{isFloat('Infinity','en-gb')}", "False")>]
[<TestCase("@{isFloat('Infinity','en-us')}", "False")>]
[<TestCase("@{isFloat('Infinity','de-de')}", "False")>]
[<TestCase("@{isFloat('inf')}", "False")>]
[<TestCase("@{isFloat('inf','')}", "False")>]
[<TestCase("@{isFloat('inf','en-gb')}", "False")>]
[<TestCase("@{isFloat('inf','en-us')}", "False")>]
[<TestCase("@{isFloat('inf','de-de')}", "False")>]
[<TestCase("@{isFloat('400,000','en-gb')}", "True")>]
[<TestCase("@{isFloat('400.000','en-gb')}", "True")>]
[<TestCase("@{isFloat('400,000','de-de')}", "True")>]
[<TestCase("@{isFloat('400.000','de-de')}", "True")>]
[<TestCase("@{isFloat('111.222.333')}", "False")>]
[<TestCase("@{isFloat('111.222.333','')}", "False")>]
[<TestCase("@{isFloat('111.222.333','en-gb')}", "False")>]
[<TestCase("@{isFloat('111.222.333','de-de')}", "True")>]
let ExecIsStringifiedFloatTest (expr: string) (expected: string) =
    testOrTrace expr <@ String expected = testExpressionEvaluation expr @>

[<TestCase("@{isFloat(3.14)}")>]
let ExecIsNotStringifiedFloatTest (expr: string) =
    raisesOrTrace<Exception> expr <@ testExpressionEvaluation expr @>
