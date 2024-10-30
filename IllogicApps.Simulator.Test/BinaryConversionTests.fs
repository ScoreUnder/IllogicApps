module IllogicApps.Simulator.Test.BinaryConversionTests

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

[<Test>]
let BinaryOfJsonTest () =
    let expected =
        jsonOf
            [ "$content-type", jsonOf "application/octet-stream"
              "$content", jsonOf "eyJoZWxsbyI6IndvcmxkIn0=" ]

    test <@ jsonsEqual expected (testExpressionEvaluation "@binary(json('{\"hello\": \"world\"}'))") @>

[<Test>]
let BinaryTest () =
    test
        <@
            testExpressionEvaluation "@binary('123asdf?a=b%20c%2F')"
            |> jsonsEqual (
                jsonOf
                    [ "$content-type", jsonOf "application/octet-stream"
                      "$content", jsonOf "MTIzYXNkZj9hPWIlMjBjJTJG" ]
            )
        @>

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
