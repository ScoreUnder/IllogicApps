module IllogicApps.Json.Test.BasicTests

open NUnit.Framework
open Swensen.Unquote
open IllogicApps.Json
open IllogicApps.Json.Conversions
open IllogicApps.Json.JsonParser

[<Test>]
let ``Parse empty string`` () = test <@ parse "\"\"" = String "" @>

[<Test>]
let ``Parse empty object`` () = test <@ parse "{}" = emptyObject @>

[<Test>]
let ``Parse empty array`` () = test <@ parse "[]" = emptyArray @>

[<Test>]
let ``Parse null`` () = test <@ parse "null" = Null @>

[<Test>]
let ``Parse true`` () = test <@ parse "true" = Boolean true @>

[<Test>]
let ``Parse false`` () =
    test <@ parse "false" = Boolean false @>

[<Test>]
let ``Parse string`` () =
    test <@ parse "\"hello\"" = String "hello" @>

[<Test>]
let ``Parse integer`` () = test <@ parse "123" = Integer 123L @>

[<Test>]
let ``Parse negative integer`` () = test <@ parse "-123" = Integer -123L @>

[<Test>]
let ``Parse float`` () =
    test <@ parse "123.456" = Float 123.456 @>

[<Test>]
let ``Parse negative float`` () =
    test <@ parse "-123.456" = Float -123.456 @>

[<Test>]
let ``Parse float with exponent`` () =
    test <@ parse "123.456e7" = Float 123.456e7 @>

[<Test>]
let ``Parse float with negative exponent`` () =
    test <@ parse "123.456e-7" = Float 123.456e-7 @>

[<Test>]
let ``Parse float with positive exponent`` () =
    test <@ parse "123.456e+7" = Float 123.456e7 @>

[<Test>]
let ``Parse string with escaped quotes`` () =
    test <@ parse "\"\\\"hello\\\"\"" = String "\"hello\"" @>

[<Test>]
let ``Parse string with escaped backslashes`` () =
    test <@ parse "\"\\\\hello\\\\\"" = String "\\hello\\" @>

[<Test>]
let ``Parse string with escaped newlines`` () =
    test <@ parse "\"\\nhello\\n\"" = String "\nhello\n" @>

[<Test>]
let ``Parse string with escaped carriage returns`` () =
    test <@ parse "\"\\rhello\\r\"" = String "\rhello\r" @>

[<Test>]
let ``Parse string with escaped tabs`` () =
    test <@ parse "\"\\thello\\t\"" = String "\thello\t" @>

[<Test>]
let ``Parse string with escaped form feeds`` () =
    test <@ parse "\"\\fhello\\f\"" = String "\fhello\f" @>

[<Test>]
let ``Parse string with escaped backspaces`` () =
    test <@ parse "\"\\bhello\\b\"" = String "\bhello\b" @>

[<Test>]
let ``Parse string with escaped unicode`` () =
    test <@ parse "\"\\u0048\\u0065\\u006c\\u006c\\u006f\"" = String "Hello" @>

[<Test>]
let ``Parse object with one key-value pair`` () =
    test <@ parse "{\"key\":\"value\"}" = createObject [ "key", String "value" ] @>

[<Test>]
let ``Parse object with multiple key-value pairs`` () =
    test
        <@
            parse "{\"key1\":\"value1\",\"key2\":\"value2\"}" = createObject
                [ "key1", String "value1"; "key2", String "value2" ]
        @>

[<Test>]
let ``Parse object with nested object`` () =
    test
        <@
            parse "{\"key\":{\"nestedKey\":\"nestedValue\"}}" = createObject
                [ "key", createObject [ "nestedKey", (String "nestedValue") ] ]
        @>

[<Test>]
let ``Parse object with nested array`` () =
    test
        <@
            parse "{\"key\":[\"value1\",\"value2\"]}" = createObject
                [ "key", createArray [| String "value1"; String "value2" |] ]
        @>

[<Test>]
let ``Parse array with one element`` () =
    test <@ parse "[\"value\"]" = createArray [ String "value" ] @>

[<Test>]
let ``Parse array with multiple elements`` () =
    test <@ parse "[\"value1\",\"value2\"]" = createArray [ String "value1"; String "value2" ] @>

[<Test>]
let ``Parse array with nested object`` () =
    test <@ parse "[{\"key\":\"value\"}]" = createArray [ createObject [ "key", (String "value") ] ] @>

[<Test>]
let ``Parse array with nested array`` () =
    test <@ parse "[[\"value1\",\"value2\"]]" = createArray [ createArray [ String "value1"; String "value2" ] ] @>

[<Test>]
let ``Parse array with two nested arrays`` () =
    test
        <@
            parse "[[\"value1\",\"value2\"],[\"value3\",\"value4\"]]" = createArray
                [ createArray [ String "value1"; String "value2" ]
                  createArray [ String "value3"; String "value4" ] ]
        @>

[<Test>]
let ``Parse array with two nested objects`` () =
    test
        <@
            parse "[{\"key1\":\"value1\"},{\"key2\":\"value2\"}]" = createArray
                [ createObject [ "key1", String "value1" ]
                  createObject [ "key2", String "value2" ] ]
        @>

[<Test>]
let ``Parse deeply nested structure`` () =
    test
        <@
            parse "{\"key1\":[{\"key2\":[{\"key3\":\"value3\"}],\"key4\":{\"arr\":[],\"obj\":{}}}]}" = createObject
                [ "key1",
                  createArray
                      [ createObject
                            [ "key2", createArray [ createObject [ "key3", (String "value3") ] ]
                              "key4", createObject [ "arr", emptyArray; "obj", emptyObject ] ] ] ]
        @>

[<Test>]
let ``Boundary case: maximum integer`` () =
    test <@ parse "9223372036854775807" = Integer 9223372036854775807L @>

[<Test>]
let ``Boundary case: minimum integer`` () =
    test <@ parse "-9223372036854775808" = Integer -9223372036854775808L @>

[<Test>]
let ``Boundary case: over maximum integer`` () =
    // Note: These float literals will both lose precision, but that's okay.
    test <@ parse "9223372036854775808" = Float 9223372036854775808.0 @>

[<Test>]
let ``Boundary case: over minimum integer`` () =
    test <@ parse "-9223372036854775809" = Float -9223372036854775809.0 @>

[<Test>]
let ``Parse invalid json: missing closing quote`` () =
    raisesWith<JsonFormatException> <@ parse "\"hello" @> (fun e -> <@ e.Message.Contains("Missing closing quote") @>)

[<Test>]
let ``Parse invalid json: missing closing brace`` () =
    raisesWith<JsonFormatException> <@ parse "{\"key\":\"value\"" @> (fun e ->
        <@ e.Message.Contains("Missing closing brace") @>)

[<Test>]
let ``Parse invalid json: missing closing bracket`` () =
    raisesWith<JsonFormatException> <@ parse "[\"value\"" @> (fun e ->
        <@ e.Message.Contains("Missing closing bracket") @>)

[<Test>]
let ``Parse invalid json: missing comma in object`` () =
    raisesWith<JsonFormatException> <@ parse "{\"key1\":\"value1\"\"key2\":\"value2\"}" @> (fun e ->
        <@ e.Message.Contains("Expecting comma") @>)

[<Test>]
let ``Parse invalid json: missing comma in array`` () =
    raises<JsonFormatException> <@ parse "[\"value1\"\"value2\"]" @>

[<Test>]
let ``Parse invalid json: missing colon in object`` () =
    raises<JsonFormatException> <@ parse "{\"key1\"\"value1\"}" @>

[<Test>]
let ``Parse invalid json: missing value in object`` () =
    raises<JsonFormatException> <@ parse "{\"key1\":}" @>

[<Test>]
let ``Parse invalid json: missing value in array start`` () =
    raises<JsonFormatException> <@ parse "[,1]" @>

[<Test>]
let ``Parse invalid json: missing value in array middle`` () =
    raises<JsonFormatException> <@ parse "[1,,2]" @>

[<Test>]
let ``Parse invalid json: missing value in array end`` () =
    raises<JsonFormatException> <@ parse "[1,]" @>

[<Test>]
let ``Parse invalid json: missing key in object`` () =
    raises<JsonFormatException> <@ parse "{:\"value\"}" @>

[<Test>]
let ``Parse invalid json: missing opening brace`` () =
    raises<JsonFormatException> <@ parse "key\":\"value\"}" @>

[<Test>]
let ``Parse invalid json: missing opening bracket`` () =
    raises<JsonFormatException> <@ parse "\"value\"]" @>

[<Test>]
let ``Parse invalid json: missing opening quote`` () =
    raises<JsonFormatException> <@ parse "hello\"" @>

[<Test>]
let ``Parse invalid json: mismatched braces`` () =
    raises<JsonFormatException> <@ parse "[{\"key\":\"value\"]}" @>

[<Test>]
let ``Parse invalid json: comma-separated key and value in object`` () =
    raises<JsonFormatException> <@ parse "{\"key1\",\"value1\"}" @>

[<Test>]
let ``Parse invalid json: colon-separated value in array`` () =
    raises<JsonFormatException> <@ parse "[\"value1\":\"value2\"]" @>

[<Test>]
let ``Parse invalid json: colon-separated pairs in object`` () =
    raises<JsonFormatException> <@ parse "{\"value1\":\"value2\":\"value3\":\"value4\"}" @>

[<Test>]
let ``Parse invalid json: number with leading zeros`` () =
    raises<JsonFormatException> <@ parse "0123" @>

[<Test>]
let ``Parse invalid json: number with leading plus sign`` () =
    raises<JsonFormatException> <@ parse "+123" @>

[<Test>]
let ``Parse invalid json: number with leading plus sign and zeros`` () =
    raises<JsonFormatException> <@ parse "+0123" @>

[<Test>]
let ``Parse invalid json: number with leading minus sign and zeros`` () =
    raises<JsonFormatException> <@ parse "-0123" @>

[<Test>]
let ``Parse invalid json: number with nothing after decimal point`` () =
    raises<JsonFormatException> <@ parse "123." @>

[<Test>]
let ``Parse invalid json: number with nothing after exponent`` () =
    raises<JsonFormatException> <@ parse "123e" @>

[<Test>]
let ``Parse invalid json: number with nothing after exponent sign`` () =
    raises<JsonFormatException> <@ parse "123e+" @>

[<Test>]
let ``Parse invalid json: number with exponent after decimal point`` () =
    raises<JsonFormatException> <@ parse "123.e+1" @>

[<Test>]
let ``Parse invalid json: number with no integral part`` () =
    raises<JsonFormatException> <@ parse ".123" @>

[<Test>]
let ``Parse invalid json: number with leading minus sign and no integral part`` () =
    raises<JsonFormatException> <@ parse "-.123" @>
