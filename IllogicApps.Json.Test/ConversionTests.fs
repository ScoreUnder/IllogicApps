module IllogicApps.Json.Test.ConversionTests

open NUnit.Framework
open Swensen.Unquote
open IllogicApps.Json
open IllogicApps.Json.Conversions

[<TestCase("Hello\nWorld\"", "Hello\\nWorld\\\"")>]
[<TestCase("Control\tCharacters", "Control\\tCharacters")>]
[<TestCase("Wingdings: \u263A", "Wingdings: \u263A")>]
[<TestCase("Sinhala: \u0D9A\u0DBA\u0DD2", "Sinhala: \u0D9A\u0DBA\u0DD2")>]
let ``escapeStringForJson should escape special characters`` (input: string, expected: string) =
    test <@ expected = escapeStringForJson input @>

[<Test>]
let ``stringOfJson should convert JsonTree to string`` () =
    let json = createObject [ "key", String "value" ]
    let expected = "{\"key\":\"value\"}"
    test <@ expected = stringOfJson json @>

[<Test>]
let ``stringOfJsonType should return correct type`` () =
    let jsonType = JsonType.String
    let expected = "string"
    test <@ expected = stringOfJsonType jsonType @>

let stringOfJsonTestCases =
    [ TestCaseData("null", Null)
      TestCaseData("true", Boolean true)
      TestCaseData("false", Boolean false)
      TestCaseData("1", Integer 1L)
      TestCaseData("1.5", Float 1.5)
      TestCaseData("\"\"", String "")
      TestCaseData("{}", emptyObject)
      TestCaseData("[]", emptyArray) ]

[<TestCaseSource("stringOfJsonTestCases")>]
let ``stringOfJson should handle different JsonTree types`` (expected: string, json: JsonTree) =
    test <@ expected = stringOfJson json @>

[<Test>]
let ``stringOfJson is allowed to serialise integral floats as if they were integers`` () =
    let json = Float 1.0
    let expected = "1"
    test <@ expected = stringOfJson json @>

[<Test>]
let ``stringOfJson should serialise large integral floats as floats`` () =
    let json = Float 1e50
    let expected = "1E+50"
    test <@ expected = stringOfJson json @>

[<Test>]
let ``stringOfJson should serialise just-too-large integral floats as floats`` () =
    let json = Float (float System.Int64.MaxValue)
    let expected = "9.223372036854776E+18"
    test <@ expected = stringOfJson json @>
