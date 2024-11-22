module IllogicApps.Json.Test.JsonSchemaTests

open NUnit.Framework
open Swensen.Unquote

open IllogicApps.Json
open IllogicApps.Json.Conversions
open IllogicApps.Json.SchemaValidator

let simpleEnumSchema =
    """
    {
        "enum": [
            1,
            2,
            "oatmeal"
        ]
    }
    """

let parsedSimpleEnumSchema =
    lazy trap <@ jsonSchemaOfJson (Parser.parse simpleEnumSchema) @>

[<Test>]
let ``Test simple enum schema is as expected after parsing`` () =
    let (Lazy data) = parsedSimpleEnumSchema

    test
        <@
            data = { emptyJsonSchema with
                       enum = Some [ Integer 1; Integer 2; String "oatmeal" ] }
        @>

let ``simple enum schema matching test cases`` =
    [ Integer 1; Integer 2; String "oatmeal" ] |> List.map TestCaseData

[<TestCaseSource(nameof ``simple enum schema matching test cases``)>]
let ``Test matching jsons against simple enum schema`` json =
    let (Lazy schema) = parsedSimpleEnumSchema

    test <@ validateJsonSchema schema json @>

let ``simple enum schema non-matching test cases`` =
    [ Null; Integer 3; String "kirby is a pink guy" ] |> List.map TestCaseData

[<TestCaseSource(nameof ``simple enum schema non-matching test cases``)>]
let ``Test non-matching jsons against simple enum schema`` json =
    let (Lazy schema) = parsedSimpleEnumSchema

    test <@ not (validateJsonSchema schema json) @>

let typeCheckedObjectAndArraySchema =
    """
    {
        "type": "object",
        "required": ["type", "value"],
        "oneOf": [
            {
                "properties": {
                    "type": { "const": "array" },
                    "value": { "type": "array" }
                }
            },
            {
                "properties": {
                    "type": { "const": "object" },
                    "value": { "type": "object" }
                }
            }
        ]
    }
    """

let parsedTypeCheckedObjectAndArraySchema =
    lazy trap <@ jsonSchemaOfJson (Parser.parse typeCheckedObjectAndArraySchema) @>

let ``type-checked object and array schema matching test cases`` =
    [ createObject [ "type", String "array"; "value", createArray [ Float 3.5; String "foo" ] ]
      createObject [ "type", String "object"; "value", createObject [ "1", Integer 2; "", Null ] ] ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``type-checked object and array schema matching test cases``)>]
let ``Test matching jsons against type-checked object and array schema`` json =
    let (Lazy schema) = parsedTypeCheckedObjectAndArraySchema

    test <@ validateJsonSchema schema json @>

let ``type-checked object and array schema non-matching test cases`` =
    [ createObject [ "type", String "array"; "value", createObject [ "1", Integer 2; "", Null ] ]
      createObject [ "type", String "object"; "value", createArray [ Float 3.5; String "foo" ] ]
      createObject [ "type", String "array"; "value", Boolean true ]
      createObject [ "type", String "object"; "value", Boolean true ]
      Null
      Boolean false
      Integer 1
      createObject [ "type", String "array" ]
      createObject [ "value", createArray [ Float 3.5; String "foo" ] ]
      createObject [ "type", String "cat"; "value", createArray [ String "meow" ] ] ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``type-checked object and array schema non-matching test cases``)>]
let ``Test non-matching jsons against type-checked object and array schema`` json =
    let (Lazy schema) = parsedTypeCheckedObjectAndArraySchema

    test <@ not (validateJsonSchema schema json) @>

let notJapaneseSchema =
    """
    {
        "not": {
            "pattern": "^(?:(?:([sc])\\1?h|([jkgsztdnhpbmyrw])\\2?y?)?[aiueo]|fu|tsu|[n ])*$"
        }
    }
    """

let parsedNotJapaneseSchema =
    lazy trap <@ jsonSchemaOfJson (Parser.parse notJapaneseSchema) @>

[<TestCase("de donde eres")>]
[<TestCase("radfahren")>]
[<TestCase("kip met rijst")>]
[<TestCase("giorno")>]
let ``Test non-Japanese strings against not-Japanese schema`` json =
    let (Lazy schema) = parsedNotJapaneseSchema

    test <@ validateJsonSchema schema (String json) @>

[<TestCase("chotto matte")>]
[<TestCase("shunkashuutou")>]
[<TestCase("shichiten hakki")>]
[<TestCase("iikagen tekitou fukaku kangaenaide")>]
let ``Test Japanese strings against not-Japanese schema`` json =
    let (Lazy schema) = parsedNotJapaneseSchema

    test <@ not (validateJsonSchema schema (String json)) @>

let ``one good turn deserves another schema`` =
    """
    {
        "dependentRequired": {
            "one good turn": ["another good turn"]
        }
    }
    """

let ``parsed one good turn deserves another schema`` =
    lazy trap <@ jsonSchemaOfJson (Parser.parse ``one good turn deserves another schema``) @>

let ``one good turn deserves another schema matching test cases`` =
    [ createObject [ "one good turn", Null; "another good turn", Integer 1 ]
      createObject
          [ "one good turn", Boolean true
            "another good turn", Integer 1
            "yet another good turn", Integer 2 ]
      createObject [ "one bad turn", Null; "another good turn", Integer 1 ]
      createObject [ "no good turns", Integer 3L ]
      createObject [ "turn around", String "every now and then i get a little etc etc" ]
      emptyObject
      Null
      String "hi"
      emptyArray ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``one good turn deserves another schema matching test cases``)>]
let ``Test matching jsons against one good turn deserves another schema`` json =
    let (Lazy schema) = ``parsed one good turn deserves another schema``

    test <@ validateJsonSchema schema json @>

let ``one good turn deserves another schema non-matching test cases`` =
    [ createObject [ "one good turn", Null ]
      createObject
          [ "one good turn", Boolean true
            "a bad turn", Integer 1
            "yet another good turn", Integer 2 ] ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``one good turn deserves another schema non-matching test cases``)>]
let ``Test non-matching jsons against one good turn deserves another schema`` json =
    let (Lazy schema) = ``parsed one good turn deserves another schema``

    test <@ not (validateJsonSchema schema json) @>

let ``array with prefix items schema`` =
    """
    {
        "prefixItems": [
            { "type": "string" },
            { "type": "integer" }
        ],
        "items": { "type": ["boolean", "null"] },
        "minItems": 4
    }
    """

let ``parsed array with prefix items schema`` =
    lazy trap <@ jsonSchemaOfJson (Parser.parse ``array with prefix items schema``) @>

let ``array with prefix items schema matching test cases`` =
    [ createArray [ String "hello"; Integer 1; Boolean true; Null ]
      createArray [ String ""; Integer 0; Null; Boolean false; Null ] ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``array with prefix items schema matching test cases``)>]
let ``Test matching jsons against array with prefix items schema`` json =
    let (Lazy schema) = ``parsed array with prefix items schema``

    test <@ validateJsonSchema schema json @>

let ``array with prefix items schema non-matching test cases`` =
    [ createArray [ String "hello"; Integer 1; Boolean true ]
      createArray [ String ""; Integer 0; Null; Boolean false; Integer 3 ]
      createArray [ String "hello"; Boolean true; Null; Boolean false ] ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``array with prefix items schema non-matching test cases``)>]
let ``Test non-matching jsons against array with prefix items schema`` json =
    let (Lazy schema) = ``parsed array with prefix items schema``

    test <@ not (validateJsonSchema schema json) @>
