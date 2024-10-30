module IllogicApps.Simulator.Test.BinaryConversionTests

open System
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
