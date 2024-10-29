module IllogicApps.Simulator.Test.BinaryConversionTests

open NUnit.Framework
open Swensen.Unquote

open JsonUtil
open TestSimUtil

[<Test>]
let JsonOfBinaryTest () =
    test
        <@
            testExpressionEvaluation "@json(binary('[1,2,3]'))"
            |> jsonsEqual (jsonOfObject [| 1; 2; 3 |])
        @>

[<Test>]
let BinaryTest () =
    test
        <@
            testExpressionEvaluation "@binary('123asdf?a=b%20c%2F')"
            |> jsonsEqual (
                !@(Map.ofList
                    [ "$content-type", !@ "application/octet-stream"
                      "$content", !@ "MTIzYXNkZj9hPWIlMjBjJTJG" ])
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
            |> jsonsEqual (!@(Map.ofList [ "$content-type", !@ "application/octet-stream"; "$content", !@ "dGVzdA==" ]))
        @>

[<Test>]
let BinaryToStringTest () =
    test
        <@
            testExpressionEvaluation "@{base64ToBinary('test')}"
            |> jsonToObject
            |> (fun f -> f :?> string)
            |> _.ToCharArray()
            |> (=) [| char 0xfffd; char 0xfffd; char 0x2d |]
        @>
