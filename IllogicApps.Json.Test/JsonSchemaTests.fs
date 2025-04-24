module IllogicApps.Json.Test.JsonSchemaTests

open System.Collections.Immutable
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
                       schema =
                           ImmutableArray.Create(
                               ("/enum", Enum(ImmutableArray.CreateRange([ Integer 1; Integer 2; String "oatmeal" ])))
                           ) }
        @>

let ``simple enum schema matching test cases`` =
    [ Integer 1; Integer 2; String "oatmeal" ] |> List.map TestCaseData

[<TestCaseSource(nameof ``simple enum schema matching test cases``)>]
let ``Test matching jsons against simple enum schema`` json =
    let (Lazy schema) = parsedSimpleEnumSchema

    test <@ validateJsonSchema schema json = { messages = []; isMatch = true } @>

let ``simple enum schema non-matching test cases`` =
    [ Null; Integer 3; String "kirby is a pink guy" ] |> List.map TestCaseData

[<TestCaseSource(nameof ``simple enum schema non-matching test cases``)>]
let ``Test non-matching jsons against simple enum schema`` json =
    let (Lazy schema) = parsedSimpleEnumSchema

    let expectedResult =
        { messages =
            [ { schemaPath = "/enum"
                jsonPath = ""
                result = Error "Enum value not correct" } ]
          isMatch = false }

    test <@ validateJsonSchema schema json = expectedResult @>

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

    test <@ validateJsonSchema schema json = { messages = []; isMatch = true } @>

let ``type-checked object and array schema non-matching test cases`` =
    [ createObject [ "type", String "array"; "value", createObject [ "1", Integer 2; "", Null ] ],
      [ { schemaPath = "/oneOf/0/properties/value/type"
          jsonPath = "/value"
          result = Error "Type not correct" }
        { schemaPath = "/oneOf/1/properties/type/const"
          jsonPath = "/type"
          result = Error "Const value not correct" }
        { schemaPath = "/oneOf"
          jsonPath = ""
          result = Error "No match" } ]
      createObject [ "type", String "object"; "value", createArray [ Float 3.5; String "foo" ] ],
      [ { schemaPath = "/oneOf/0/properties/type/const"
          jsonPath = "/type"
          result = Error "Const value not correct" }
        { schemaPath = "/oneOf/1/properties/value/type"
          jsonPath = "/value"
          result = Error "Type not correct" }
        { schemaPath = "/oneOf"
          jsonPath = ""
          result = Error "No match" } ]
      createObject [ "type", String "array"; "value", Boolean true ],
      [ { schemaPath = "/oneOf/0/properties/value/type"
          jsonPath = "/value"
          result = Error "Type not correct" }
        { schemaPath = "/oneOf/1/properties/value/type"
          jsonPath = "/value"
          result = Error "Type not correct" }
        { schemaPath = "/oneOf/1/properties/type/const"
          jsonPath = "/type"
          result = Error "Const value not correct" }
        { schemaPath = "/oneOf"
          jsonPath = ""
          result = Error "No match" } ]
      createObject [ "type", String "object"; "value", Boolean true ],
      [ { schemaPath = "/oneOf/0/properties/value/type"
          jsonPath = "/value"
          result = Error "Type not correct" }
        { schemaPath = "/oneOf/0/properties/type/const"
          jsonPath = "/type"
          result = Error "Const value not correct" }
        { schemaPath = "/oneOf/1/properties/value/type"
          jsonPath = "/value"
          result = Error "Type not correct" }
        { schemaPath = "/oneOf"
          jsonPath = ""
          result = Error "No match" } ]
      Null,
      [ { schemaPath = "/oneOf"
          jsonPath = ""
          result = Error "More than one match" }
        { schemaPath = "/type"
          jsonPath = ""
          result = Error "Type not correct" } ]
      Boolean false,
      [ { schemaPath = "/oneOf"
          jsonPath = ""
          result = Error "More than one match" }
        { schemaPath = "/type"
          jsonPath = ""
          result = Error "Type not correct" } ]
      Integer 1,
      [ { schemaPath = "/oneOf"
          jsonPath = ""
          result = Error "More than one match" }
        { schemaPath = "/type"
          jsonPath = ""
          result = Error "Type not correct" } ]
      createObject [ "type", String "array" ],
      [ { schemaPath = "/required"
          jsonPath = ""
          result = Error "Object is missing required properties" } ]
      createObject [ "value", createArray [ Float 3.5; String "foo" ] ],
      [ { schemaPath = "/required"
          jsonPath = ""
          result = Error "Object is missing required properties" } ]
      createObject [ "type", String "cat"; "value", createArray [ String "meow" ] ],
      [ { schemaPath = "/oneOf/0/properties/type/const"
          jsonPath = "/type"
          result = Error "Const value not correct" }
        { schemaPath = "/oneOf/1/properties/value/type"
          jsonPath = "/value"
          result = Error "Type not correct" }
        { schemaPath = "/oneOf/1/properties/type/const"
          jsonPath = "/type"
          result = Error "Const value not correct" }
        { schemaPath = "/oneOf"
          jsonPath = ""
          result = Error "No match" } ] ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``type-checked object and array schema non-matching test cases``)>]
let ``Test non-matching jsons against type-checked object and array schema`` json errors =
    let (Lazy schema) = parsedTypeCheckedObjectAndArraySchema

    test
        <@
            let result = validateJsonSchema schema json

            let expected =
                { messages = List.sort errors
                  isMatch = false }

            let result =
                { result with
                    messages = List.sort result.messages }

            result = expected
        @>

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

    test <@ validateJsonSchema schema (String json) = { messages = []; isMatch = true } @>

[<TestCase("chotto matte")>]
[<TestCase("shunkashuutou")>]
[<TestCase("shichiten hakki")>]
[<TestCase("iikagen tekitou fukaku kangaenaide")>]
let ``Test Japanese strings against not-Japanese schema`` json =
    let (Lazy schema) = parsedNotJapaneseSchema

    let expectedResult =
        { messages =
            [ { schemaPath = "/not"
                jsonPath = ""
                result = Error "should not have validated, but did" } ]
          isMatch = false }

    test <@ validateJsonSchema schema (String json) = expectedResult @>

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

    test <@ validateJsonSchema schema json = { messages = []; isMatch = true } @>

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

    let expectedResult =
        { messages =
            [ { schemaPath = "/dependentRequired/one good turn"
                jsonPath = "/another good turn"
                result = Error "Object is missing required properties" } ]
          isMatch = false }

    test <@ validateJsonSchema schema json = expectedResult @>

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
      createArray [ String ""; Integer 0; Null; Boolean false; Null ]
      String "who says it has to be an array" ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``array with prefix items schema matching test cases``)>]
let ``Test matching jsons against array with prefix items schema`` json =
    let (Lazy schema) = ``parsed array with prefix items schema``

    test <@ validateJsonSchema schema json = { messages = []; isMatch = true } @>

let ``array with prefix items schema non-matching test cases`` =
    [ createArray [ String "hello"; Integer 1; Boolean true ],
      { schemaPath = "/minItems"
        jsonPath = ""
        result = Error "Array is too short" }
      createArray [ String ""; Integer 0; Null; Boolean false; Integer 3 ],
      { schemaPath = "/items/type"
        jsonPath = "/4"
        result = Error "Type not correct" }
      createArray [ String "hello"; Boolean true; Null; Boolean false ],
      { schemaPath = "/prefixItems/1/type"
        jsonPath = "/1"
        result = Error "Type not correct" } ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``array with prefix items schema non-matching test cases``)>]
let ``Test non-matching jsons against array with prefix items schema`` json error =
    let (Lazy schema) = ``parsed array with prefix items schema``

    let expectedResult =
        { messages = [ error ]
          isMatch = false }

    test <@ validateJsonSchema schema json = expectedResult @>


let ``refs test schema`` =
    """
    {
        "type": "object",
        "properties": {
            "fail": { "$ref": "#/$defs/fail" },
            "recursive": { "$ref": "#" },
            "foo": { "$ref": "#/$defs/foo" }
        },
        "$defs": {
            "fail": {
                "const": false
            },
            "foo": {
                "type": "array",
                "items": {
                    "$ref": "#/$defs/foo"
                }
            }
        }
    }
    """

let ``parsed refs test schema`` =
    lazy trap <@ jsonSchemaOfJson (Parser.parse ``refs test schema``) @>

let ``refs test schema matching test cases`` =
    [ emptyObject
      createObject [ "fail", Boolean false; "recursive", emptyObject; "foo", emptyArray ]
      createObject [ "foo", createArray [ emptyArray; createArray [ emptyArray ]; emptyArray ] ]
      createObject [ "recursive", createObject [ "recursive", emptyObject ] ] ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``refs test schema matching test cases``)>]
let ``Test matching jsons against refs test schema`` json =
    let (Lazy schema) = ``parsed refs test schema``

    let expectedResult = { messages = []; isMatch = true }
    test <@ validateJsonSchema schema json = expectedResult @>

let ``refs test schema non-matching test cases`` =
    [ createObject [ "fail", Boolean true; "recursive", emptyObject; "foo", emptyArray ],
      { schemaPath = "/$defs/fail/const"
        jsonPath = "/fail"
        result = Error "Const value not correct" }
      createObject [ "foo", createArray [ emptyArray; createArray [ emptyObject ]; emptyArray ] ],
      { schemaPath = "/$defs/foo/type"
        jsonPath = "/foo/1/0"
        result = Error "Type not correct" }
      createObject [ "recursive", createObject [ "recursive", createObject [ "fail", Boolean true ] ] ],
      { schemaPath = "/$defs/fail/const"
        jsonPath = "/recursive/recursive/fail"
        result = Error "Const value not correct" }
      String "string",
      { schemaPath = "/type"
        jsonPath = ""
        result = Error "Type not correct" } ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``refs test schema non-matching test cases``)>]
let ``Test non-matching jsons against refs test schema`` json error =
    let (Lazy schema) = ``parsed refs test schema``

    let expectedResult =
        { messages = [ error ]
          isMatch = false }

    test <@ validateJsonSchema schema json = expectedResult @>
