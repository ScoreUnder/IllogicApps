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

[<TestCase("@base64ToBinary(base64('test'))", "dGVzdA==")>]
[<TestCase("@base64ToBinary('')", "")>]
[<TestCase("@base64ToBinary('aGVsbG8 gd29ybGQ=')", "aGVsbG8 gd29ybGQ=")>]
[<TestCase("@base64ToBinary('aGVsbG8gd29ybGQ=')", "aGVsbG8gd29ybGQ=")>]
let Base64ToBinaryTest expr (expectedBase64: string) =
    let expected =
        jsonOf
            [ "$content-type", jsonOf "application/octet-stream"
              "$content", jsonOf expectedBase64 ]

    test <@ jsonsEqual expected (testExpressionEvaluation expr) @>

[<TestCase("@base64ToString('aGVsbG8gd29ybGQ=')", "hello world")>]
let Base64ToStringTest (expr: string) (expected: string) =
    test <@ expected.Equals(jsonToObject (testExpressionEvaluation expr)) @>

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

[<TestCase("@base64ToBinary('$#[<')")>]
[<TestCase("@base64ToBinary('aa')")>]
let InvalidBase64ToBinaryTest (expr: string) =
    raisesOrTrace<Exception> expr <@ testExpressionEvaluation expr @>

[<TestCase("@dataUriToBinary('data:text/plain;charset=utf-8;base64,aGVsbG8=')",
           """{"$content-type":"text/plain;charset=utf-8","$content":"aGVsbG8="}""")>]
[<TestCase("@dataUriToBinary('data:text/plain;charset=utf-8,aGVsbG8=')",
           """{"$content-type":"text/plain;charset=utf-8","$content":"YUdWc2JHOD0="}""")>]
[<TestCase("@dataUriToBinary('data:text/plain;charset=utf-8;base64,aGVsbG8%20gd29ybGQ=')",
           "{\"$content-type\":\"text/plain;charset=utf-8\",\"$content\":\"aGVsbG8%20gd29ybGQ=\"}")>]
[<TestCase("@dataUriToBinary('data:text/plain;charset=utf-8;base64,aGVsbG8gd29ybGQ=')",
           "{\"$content-type\":\"text/plain;charset=utf-8\",\"$content\":\"aGVsbG8gd29ybGQ=\"}")>]
[<TestCase("@dataUriToBinary('data:text/plain;charset=utf-8;base64,aGVsbG8g+d29ybGQ=')",
           "{\"$content-type\":\"text/plain;charset=utf-8\",\"$content\":\"aGVsbG8g+d29ybGQ=\"}")>]
[<TestCase("@dataUriToBinary('data:text/plain;charset=utf-8;base64,aGVsbG8g d29ybGQ=')",
           "{\"$content-type\":\"text/plain;charset=utf-8\",\"$content\":\"aGVsbG8g d29ybGQ=\"}")>]
[<TestCase("@dataUriToBinary('data:text/plain;charset=utf-8,abc+def ghi+jkl=asdf+qwer?abc+def=ghi+jkl')",
           "{\"$content-type\":\"text/plain;charset=utf-8\",\"$content\":\"YWJjIGRlZiBnaGkgamtsPWFzZGYgcXdlcj9hYmMgZGVmPWdoaSBqa2w=\"}")>]
[<TestCase("@dataUriToBinary('data:text/plain;charset=utf-8;base64,JTIyaGVsbG8rd29ybGQlMjIlM0YlM2Y=')",
           "{\"$content-type\":\"text/plain;charset=utf-8\",\"$content\":\"JTIyaGVsbG8rd29ybGQlMjIlM0YlM2Y=\"}")>]
[<TestCase("@dataUriToBinary('data:bad/mime;charset=utf-8,  [ 1, 2, 3 ]  ')",
           "{\"$content-type\":\"bad/mime;charset=utf-8\",\"$content\":\"ICBbIDEsIDIsIDMgXSAg\"}")>]
[<TestCase("@dataUriToBinary('data:,  [ 1, 2, 3 ]  ')",
           "{\"$content-type\":\"text/plain\",\"$content\":\"ICBbIDEsIDIsIDMgXSAg\"}")>]
[<TestCase("@dataUriToBinary('data:base64,dGVzdA==')", "{\"$content-type\":\"text/plain\",\"$content\":\"dGVzdA==\"}")>]
[<TestCase("@dataUriToBinary('data:;base64,dGVzdA==')", "{\"$content-type\":\"text/plain\",\"$content\":\"dGVzdA==\"}")>]
[<TestCase("@dataUriToBinary('data:text/plain,  [ 1, 2, 3 ]  ')",
           "{\"$content-type\":\"text/plain\",\"$content\":\"ICBbIDEsIDIsIDMgXSAg\"}")>]
[<TestCase("@dataUriToBinary('data:text/plain;base64;charset=ascii,dGVzdA==')",
           "{\"$content-type\":\"text/plain;base64;charset=ascii\",\"$content\":\"ZEdWemRBPT0=\"}")>]
[<TestCase("@dataUriToBinary('data:text/plain;cows=moo;charset=ascii;base64,dGVzdA==')",
           "{\"$content-type\":\"text/plain;cows=moo;charset=ascii\",\"$content\":\"dGVzdA==\"}")>]
[<TestCase("@dataUriToBinary('data:text/plain;charset=ascii;cats=cute;base64,dGVzdA==')",
           "{\"$content-type\":\"text/plain;charset=ascii;cats=cute\",\"$content\":\"dGVzdA==\"}")>]
[<TestCase("@dataUriToBinary('data:text/plain;charset=utf-8;cats=cute;base64,dGVzdA==')",
           "{\"$content-type\":\"text/plain;charset=utf-8;cats=cute\",\"$content\":\"dGVzdA==\"}")>]
[<TestCase("@dataUriToBinary('data:text/plain;charset=utf-8;base64,dGVzdA==')",
           "{\"$content-type\":\"text/plain;charset=utf-8\",\"$content\":\"dGVzdA==\"}")>]
[<TestCase("@dataUriToBinary('data:text/plain;cows=moo;charset=utf-8;cats=cute;base64,dGVzdA==')",
           "{\"$content-type\":\"text/plain;cows=moo;charset=utf-8;cats=cute\",\"$content\":\"dGVzdA==\"}")>]
[<TestCase("@dataUriToBinary('DATA:TEXT/PLAIN;COWS=MOO;CHARSET=UTF-8;CATS=CUTE;BASE64,dGVzdA==')",
           "{\"$content-type\":\"TEXT/PLAIN;COWS=MOO;CHARSET=UTF-8;CATS=CUTE\",\"$content\":\"dGVzdA==\"}")>]
let DataUriToBinaryTest (expr: string) (expected: string) =
    let expected = JsonSerializer.Deserialize<JsonNode>(expected)
    testOrTrace expr <@ jsonsEqual expected (testExpressionEvaluation expr) @>

[<TestCase("@dataUriToString('data:text/plain;charset=utf-8,%22hello,%20world%22')", "\"hello, world\"")>]
[<TestCase("@dataUriToString('data:text/plain;charset=utf-8;base64,aGVsbG8 gd29ybGQ=')", "hello world")>]
[<TestCase("@dataUriToString('data:text/plain;test=wow;charset=nope;base64,aGVsbG8gd29ybGQ=')", "hello world")>]
[<TestCase("@dataUriToString('data:text/plain;charset=utf-8,abc+def ghi+jkl=asdf+qwer?abc+def=ghi+jkl')",
           "abc def ghi jkl=asdf qwer?abc def=ghi jkl")>]
[<TestCase("@dataUriToString('data:text/plain;charset=utf-8;base64,JTIyaGVsbG8rd29ybGQlMjIlM0YlM2Y=')",
           "%22hello+world%22%3F%3f")>]
[<TestCase("@dataUriToString('data:badmime;charset=utf-8,  [ 1, 2, 3 ]  ')", "  [ 1, 2, 3 ]  ")>]
[<TestCase("@dataUriToString('data:bad/mime;charset=utf-8,  [ 1, 2, 3 ]  ')", "  [ 1, 2, 3 ]  ")>]
[<TestCase("@dataUriToString('data:,  [ 1, 2, 3 ]  ')", "  [ 1, 2, 3 ]  ")>]
[<TestCase("@dataUriToString('data:base64,dGVzdA==')", "test")>]
[<TestCase("@dataUriToString('data:;base64,dGVzdA==')", "test")>]
[<TestCase("@dataUriToString('data:text/plain,  [ 1, 2, 3 ]  ')", "  [ 1, 2, 3 ]  ")>]
[<TestCase("@dataUriToString('data:text/plain;cows=moo;base64;charset=ascii;cats=cute=very,dGVzdA==')", "dGVzdA==")>]
[<TestCase("@dataUriToString('data:text/plain;base64;charset=ascii,dGVzdA==')", "dGVzdA==")>]
[<TestCase("@dataUriToString('data:text/plain;cows=moo;charset=ascii;base64,dGVzdA==')", "test")>]
[<TestCase("@dataUriToString('data:text/plain;charset=utf-8;cats=cute;base64,dGVzdA==')", "test")>]
[<TestCase("@dataUriToString('data:text/plain;charset=utf-8;base64,dGVzdA==')", "test")>]
[<TestCase("@dataUriToString('data:text/plain;cows=moo;charset=utf-8;cats=cute;base64,dGVzdA==')", "test")>]
[<TestCase("@dataUriToString('DATA:TEXT/PLAIN;COWS=MOO;CHARSET=UTF-8;CATS=CUTE;BASE64,dGVzdA==')", "test")>]
let DataUriToStringTest expr (expected: string) =
    testOrTrace expr <@ expected.Equals(jsonToObject (testExpressionEvaluation expr)) @>

[<TestCase("@{dataUriToBinary('data:text/plain;charset=utf-8,abc+def ghi+jkl=asdf+qwer?abc+def=ghi+jkl')}",
           "abc def ghi jkl=asdf qwer?abc def=ghi jkl")>]
[<TestCase("@{dataUriToBinary('data:text/plain;charset=utf-8;base64,JTIyaGVsbG8rd29ybGQlMjIlM0YlM2Y=')}",
           "%22hello+world%22%3F%3f")>]
[<TestCase("@{dataUriToBinary('data:text/plain;charset=utf-8;base64,aGVsbG8g d29ybGQ=')}", "hello world")>]
[<TestCase("@{dataUriToBinary('data:bad/mime;charset=utf-8,  [ 1, 2, 3 ]  ')}", "  [ 1, 2, 3 ]  ")>]
[<TestCase("@{dataUriToBinary('data:,  [ 1, 2, 3 ]  ')}", "  [ 1, 2, 3 ]  ")>]
[<TestCase("@{dataUriToBinary('data:base64,dGVzdA==')}", "test")>]
[<TestCase("@{dataUriToBinary('data:;base64,dGVzdA==')}", "test")>]
[<TestCase("@{dataUriToBinary('data:text/plain,  [ 1, 2, 3 ]  ')}", "  [ 1, 2, 3 ]  ")>]
[<TestCase("@{dataUriToBinary('data:text/plain;cows=moo;charset=ascii;base64,dGVzdA==')}", "test")>]
[<TestCase("@{dataUriToBinary('data:text/plain;charset=ascii;cats=cute;base64,dGVzdA==')}", "test")>]
[<TestCase("@{dataUriToBinary('data:text/plain;cows=moo;charset=utf-8;cats=cute;base64,dGVzdA==')}", "test")>]
[<TestCase("@{dataUriToBinary('data:text/plain;charset=utf-8;cats=cute;base64,dGVzdA==')}", "test")>]
[<TestCase("@{dataUriToBinary('data:text/plain;charset=utf-8;base64,dGVzdA==')}", "test")>]
[<TestCase("@{dataUriToBinary('DATA:TEXT/PLAIN;COWS=MOO;CHARSET=UTF-8;CATS=CUTE;BASE64,dGVzdA==')}", "test")>]
let StringifiedDataUriToBinaryTest expr (expected: string) =
    testOrTrace expr <@ expected.Equals(jsonToObject (testExpressionEvaluation expr)) @>

[<TestCase("@dataUriToBinary('data:;;base64,dGVzdA==')")>] // too many semicolons
[<TestCase("@dataUriToBinary('data:;,test')")>] // too many semicolons
[<TestCase("@dataUriToBinary('data:;;,test')")>] // too many semicolons
[<TestCase("@dataUriToBinary('data:badmime;charset=utf-8,  [ 1, 2, 3 ]  ')")>] // MIME is not formatted correctly
[<TestCase("@dataUriToBinary('data:text/plain/epic;charset=utf-8;base64,aGVsbG8g d29ybGQ=')")>] // MIME is not formatted correctly
[<TestCase("@dataUriToBinary('data:application/json;charset=fake-encoding,  [ 1, 2, 3 ]  ')")>] // encoding is wrong
[<TestCase("@dataUriToBinary('data:text/plain;charset=martian;base64,aGVsbG8gd29ybGQ=')")>] // encoding is wrong, but already base64
[<TestCase("@dataUriToBinary('data:text/plain;cows=moo;base64;charset=ascii;cats=cute=very,dGVzdA==')")>] // double equals sign in parameter, base64 not at end
let InvalidDataUriToBinaryTest (expr: string) =
    raisesOrTrace<Exception> expr <@ testExpressionEvaluation expr @>

[<TestCase("@{dataUriToBinary('data:;;base64,dGVzdA==')}")>] // too many semicolons
[<TestCase("@{dataUriToBinary('data:;,test')}")>] // too many semicolons
[<TestCase("@{dataUriToBinary('data:;;,test')}")>] // too many semicolons
[<TestCase("@{dataUriToBinary('data:badmime;charset=utf-8,  [ 1, 2, 3 ]  ')}")>] // MIME is not formatted correctly
[<TestCase("@{dataUriToBinary('data:text/plain/epic;charset=utf-8;base64,aGVsbG8g d29ybGQ=')}")>] // MIME is not formatted correctly
[<TestCase("@{dataUriToBinary('data:application/json;charset=fake-encoding,  [ 1, 2, 3 ]  ')}")>] // encoding is wrong
[<TestCase("@{dataUriToBinary('data:text/plain;cows=moo;base64;charset=ascii;cats=cute=very,dGVzdA==')}")>] // double equals sign in parameter
[<TestCase("@{dataUriToBinary('data:text/plain;base64;charset=ascii,dGVzdA==')}")>] // base64 is not last
[<TestCase("@{dataUriToBinary('data:text/plain;charset=utf-8;base64,aGVsbG8%20gd29ybGQ=')}")>] // base64 is invalid
let InvalidStringifiedDataUriToBinaryTest (expr: string) =
    raisesOrTrace<Exception> expr <@ testExpressionEvaluation expr @>

[<TestCase("@dataUriToString('data:;;base64,dGVzdA==')")>] // too many semicolons
[<TestCase("@dataUriToString('data:;,test')")>] // too many semicolons
[<TestCase("@dataUriToString('data:;;,test')")>] // too many semicolons
[<TestCase("@dataUriToString('data:application/json;charset=fake-encoding,  [ 1, 2, 3 ]  ')")>] // encoding is wrong
[<TestCase("@dataUriToString('data:text/plain;charset=ascii;cats=cute;base64,dGVzdA==')")>] // Seems to think any alternative charset is wrong?
[<TestCase("@dataUriToString('data:text/plain;charset=ascii;base64,dGVzdA==')")>] // ditto
[<TestCase("@dataUriToString('data:text/plain;CHARSET=nope;base64,aGVsbG8gd29ybGQ=')")>] // ditto but charset tag is uppercase
[<TestCase("@dataUriToString('data:text/plain;charset=utf-8;base64,aGVsbG8%20gd29ybGQ=')")>] // base64 is invalid
let InvalidDataUriToStringTest (expr: string) =
    raisesOrTrace<Exception> expr <@ testExpressionEvaluation expr @>

[<TestCase("@dataUri('test')", "data:text/plain;charset=utf-8;base64,dGVzdA==")>]
[<TestCase("@dataUri(xml('<t/>'))", "data:application/xml;charset=utf-8;base64,PHQgLz4=")>]
[<TestCase("@dataUri(json('{\"a\":3}'))", "data:application/json;charset=utf-8;base64,eyJhIjozfQ==")>]
[<TestCase("@dataUri(createArray(1,2,3))", "data:application/json;charset=utf-8;base64,WzEsMiwzXQ==")>]
[<TestCase("@{dataUri(1)}", "data:application/json;charset=utf-8;base64,MQ==")>]
[<TestCase("@{dataUri(true)}", "data:application/json;charset=utf-8;base64,VHJ1ZQ==")>]
let DataUriTest (expr: string) (expected: string) =
    test <@ expected.Equals(jsonToObject (testExpressionEvaluation expr)) @>

[<TestCase("@{dataUri(null)}")>]
let InvalidDataUriTest (expr: string) =
    raisesOrTrace<Exception> expr <@ testExpressionEvaluation expr @>
