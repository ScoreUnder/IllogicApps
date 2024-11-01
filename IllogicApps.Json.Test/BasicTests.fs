module IllogicApps.Json.Test

open System.Collections.Immutable
open NUnit.Framework
open Swensen.Unquote
open IllogicApps.Json.Parser

[<Test>]
let ``Parse empty string`` () = test <@ parse "\"\"" = String "" @>

[<Test>]
let ``Parse empty object`` () =
    test <@ parse "{}" = Object Map.empty @>

[<Test>]
let ``Parse empty array`` () =
    test <@ parse "[]" = Array ImmutableArray.Empty @>

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
    test <@ parse "{\"key\":\"value\"}" = Object(Map.ofList [ "key", (String "value") ]) @>

[<Test>]
let ``Parse object with multiple key-value pairs`` () =
    test
        <@
            parse "{\"key1\":\"value1\",\"key2\":\"value2\"}" = Object(
                Map.ofList [ "key1", (String "value1"); "key2", (String "value2") ]
            )
        @>

[<Test>]
let ``Parse object with nested object`` () =
    test
        <@
            parse "{\"key\":{\"nestedKey\":\"nestedValue\"}}" = Object(
                Map.ofList [ "key", Object(Map.ofList [ "nestedKey", (String "nestedValue") ]) ]
            )
        @>

[<Test>]
let ``Parse object with nested array`` () =
    test
        <@
            parse "{\"key\":[\"value1\",\"value2\"]}" = Object(
                Map.ofList [ "key", Array(ImmutableArray.Create(String "value1", String "value2")) ]
            )
        @>

[<Test>]
let ``Parse array with one element`` () =
    test <@ parse "[\"value\"]" = Array(ImmutableArray.Create(String "value")) @>

[<Test>]
let ``Parse array with multiple elements`` () =
    test <@ parse "[\"value1\",\"value2\"]" = Array(ImmutableArray.Create(String "value1", String "value2")) @>

[<Test>]
let ``Parse array with nested object`` () =
    test
        <@ parse "[{\"key\":\"value\"}]" = Array(ImmutableArray.Create(Object(Map.ofList [ "key", (String "value") ]))) @>

[<Test>]
let ``Parse array with nested array`` () =
    test
        <@
            parse "[[\"value1\",\"value2\"]]" = Array(
                ImmutableArray.Create(Array(ImmutableArray.Create(String "value1", String "value2")))
            )
        @>

[<Test>]
let ``Parse array with two nested arrays`` () =
    test
        <@
            parse "[[\"value1\",\"value2\"],[\"value3\",\"value4\"]]" = Array(
                ImmutableArray.Create(
                    Array(ImmutableArray.Create(String "value1", String "value2")),
                    Array(ImmutableArray.Create(String "value3", String "value4"))
                )
            )
        @>

[<Test>]
let ``Parse array with two nested objects`` () =
    test
        <@
            parse "[{\"key1\":\"value1\"},{\"key2\":\"value2\"}]" = Array(
                ImmutableArray.Create(
                    Object(Map.ofList [ "key1", (String "value1") ]),
                    Object(Map.ofList [ "key2", (String "value2") ])
                )
            )
        @>

[<Test>]
let ``Parse deeply nested structure`` () =
    test
        <@
            parse "{\"key1\":[{\"key2\":[{\"key3\":\"value3\"}],\"key4\":{\"arr\":[],\"obj\":{}}}]}" = Object(
                Map.ofList
                    [ "key1",
                      Array(
                          ImmutableArray.Create(
                              Object(
                                  Map.ofList
                                      [ "key2",
                                        Array(ImmutableArray.Create(Object(Map.ofList [ "key3", (String "value3") ])))
                                        "key4",
                                        Object(
                                            Map.ofList [ "arr", Array ImmutableArray.Empty; "obj", Object Map.empty ]
                                        ) ]
                              )
                          )
                      ) ]
            )
        @>

[<Test>]
[<Ignore("Intentionally slow")>]
let ``Parse deeply nested structure a million times`` () =
    for i in 1..1_000_000 do
        parse "{\"key1\":[{\"key2\":[{\"key3\":\"value3\"}],\"key4\":{\"arr\":[],\"obj\":{}}}]}" |> ignore
