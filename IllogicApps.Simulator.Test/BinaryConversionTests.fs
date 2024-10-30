module IllogicApps.Simulator.Test.BinaryConversionTests

open System
open System.Text.Json
open System.Text.Json.Nodes
open NUnit.Framework
open Swensen.Unquote

open IllogicApps.Core.JsonUtil
open TestSimUtil

[<Test>]
let JsonOfBinaryTest () =
    test
        <@
            testExpressionEvaluation "@json(binary('[1,2,3]'))"
            |> jsonsEqual (jsonOf [| jsonOf 1L; jsonOf 2L; jsonOf 3L |])
        @>

[<TestCase("@binary(json('{\"hello\": \"world\"}'))", "eyJoZWxsbyI6IndvcmxkIn0=")>]
[<TestCase("@binary(3.14)", "My4xNA==")>]
[<TestCase("@binary('123asdf?a=b%20c%2F')", "MTIzYXNkZj9hPWIlMjBjJTJG")>]
[<TestCase("@binary(binary('123'))", "MTIz")>]
[<TestCase("@binary(createArray('a','b','c'))", "WyJhIiwiYiIsImMiXQ==")>]
[<TestCase("@binary(true)", "VHJ1ZQ==")>]
[<TestCase("@binary(false)", "RmFsc2U=")>]
let BinaryOfTest (expr: string) (expected: string) =
    let expected =
        jsonOf
            [ "$content-type", jsonOf "application/octet-stream"
              "$content", jsonOf expected ]

    test <@ jsonsEqual expected (testExpressionEvaluation expr) @>

[<Test>]
let Base64Test () =
    test <@ testExpressionEvaluation "@base64('test')" |> jsonToObject |> "dGVzdA==".Equals @>

[<Test>]
let Base64RoundTripTest () =
    test
        <@
            testExpressionEvaluation "@base64ToString(base64('test'))"
            |> jsonToObject
            |> "test".Equals
        @>

[<Test>]
let Base64ToBinaryTest () =
    test
        <@
            testExpressionEvaluation "@base64ToBinary(base64('test'))"
            |> jsonsEqual (
                jsonOf
                    [ "$content-type", jsonOf "application/octet-stream"
                      "$content", jsonOf "dGVzdA==" ]
            )
        @>

[<Test>]
let BinaryToStringTest () =
    test
        <@
            testExpressionEvaluation "@{base64ToBinary('test')}"
            |> _.GetValue<string>().ToCharArray()
            |> (=) [| char 0xfffd; char 0xfffd; char 0x2d |]
        @>

[<TestCase("@binary(null)")>]
let BinaryOfInvalidTypeTest (expr: string) =
    raisesOrTrace<Exception> expr <@ testExpressionEvaluation expr @>

[<TestCase("@dataUriToBinary('data:text/plain;charset=utf-8;base64,aGVsbG8=')",
           """{"$content-type":"text/plain;charset=utf-8","$content":"aGVsbG8="}""")>]
[<TestCase("@dataUriToBinary('data:text/plain;charset=utf-8,aGVsbG8=')",
           """{"$content-type":"text/plain;charset=utf-8","$content":"YUdWc2JHOD0="}""")>]
let DataUriToBinaryTest (expr: string) (expected: string) =
    let expected = JsonSerializer.Deserialize<JsonNode>(expected)
    test <@ jsonsEqual expected (testExpressionEvaluation expr) @>

[<TestCase("@dataUri('test')", "data:text/plain;charset=utf-8;base64,dGVzdA==")>]
[<TestCase("@dataUri(xml('<t/>'))", "data:application/xml;charset=utf-8;base64,PHQgLz4=")>]
[<TestCase("@dataUri(json('{\"a\":3}'))", "data:application/json;charset=utf-8;base64,eyJhIjozfQ==")>]
[<TestCase("@dataUri(createArray(1,2,3))", "data:application/json;charset=utf-8;base64,WzEsMiwzXQ==")>]
let DataUriTest (expr: string) (expected: string) =
    test <@ expected.Equals(jsonToObject (testExpressionEvaluation expr)) @>
