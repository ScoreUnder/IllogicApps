module IllogicApps.Simulator.Test.ManipulationFunctionsTests

open NUnit.Framework
open Swensen.Unquote
open IllogicApps.Json
open TestSimUtil

let ``coalesce test cases`` =
    [ "@coalesce(null)", Ok Null // coalesce of one null
      "@coalesce(true)", Ok(Boolean true) // coalesce of one non-null
      "@coalesce()", Error "at least one parameter" // empty coalesce
      "@coalesce(null,'x')", Ok(String "x") // coalesce with first parameter null
      "@coalesce('x',null)", Ok(String "x") // coalesce with second parameter null
      "@coalesce(null,null)", Ok Null // coalesce with two nulls
      "@coalesce('x','y')", Ok(String "x") // coalesce with two different parameters
      "@coalesce(null,null,null,null,null,null,null,'hi',null)", Ok(String "hi") ] // coalesce with one non-null hiding amongst many nulls
    |> List.map TestCaseData

[<TestCaseSource(nameof ``coalesce test cases``)>]
let ``Test coalesce`` expr expected = jsonOrFailTest expr expected

let ``coalesce short-circuiting test cases`` =
    [ "@coalesce('hi',FakeFunctionWhichErrors())", Ok(String "hi")
      "@coalesce(null,FakeFunctionWhichErrors())",
      Error "The template function 'FakeFunctionWhichErrors' is not defined or not valid"
      "@coalesce(FakeFunctionWhichErrors(),'hi')",
      Error "The template function 'FakeFunctionWhichErrors' is not defined or not valid"
      "@coalesce(FakeFunctionWhichErrors(),null)",
      Error "The template function 'FakeFunctionWhichErrors' is not defined or not valid" ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``coalesce short-circuiting test cases``)>]
let ``Test coalesce short-circuiting`` expr expected = jsonOrFailTest expr expected

let ``addProperty test cases`` =
    [ "@addProperty(json('{}'),'a','')", Ok """{ "a": "" }"""
      "@addProperty(json('{\"a\":1,\"b\":2,\"c\":3,\"d\":4}'),'c',3)", Error "property to not exist in the object"
      "@addProperty(json('{\"a\":1,\"b\":2,\"c\":3,\"d\":4}'),'c',999)", Error "property to not exist in the object"
      "@addProperty(json('{\"a\":1,\"b\":2,\"d\":4}'),'c',3)", Ok """{ "a": 1, "b": 2, "d": 4, "c": 3 }"""
      "@addProperty(json('{\"a\":1,\"b\":2,\"c\":3,\"d\":4}'),'',null)",
      Ok """{ "a": 1, "b": 2, "c": 3, "d": 4, "": null }"""
      "@addProperty(json('{\"a\":1,\"b\":2,\"c\":3,\"d\":4}'),null,'')", Error "be of type string"
      "@addProperty(json('{}'),1,1)", Error "be of type string"
      "@addProperty(json('{}'),false,1)", Error "be of type string"
      "@addProperty(json('{}'),json('[]'),1)", Error "be of type string"
      "@addProperty(json('{}'),json('[\"a\"]'),1)", Error "be of type string"
      "@addProperty(json('{}'),'value','a','b')", Error "expects three parameters"
      "@addProperty(json('{}'),'value')", Error "expects three parameters"
      "@addProperty(null,'x',1)", Error "be of type object"
      "@addProperty(true,'x',1)", Error "be of type object"
      "@addProperty(1,'x',1)", Error "be of type object"
      "@addProperty('abc','x',1)", Error "be of type object"
      "@addProperty('{}','x',1)", Error "be of type object"
      "@addProperty(createArray(1,2,3),'x',1)", Error "be of type object" ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``addProperty test cases``)>]
let ``Test addProperty`` expr expected = objOrFailTest expr expected

let ``addProperty binary/xml test cases`` =
    [ "@addProperty(binary('test'),'x',1)",
      """{ "$content-type": "application/octet-stream", "$content": "dGVzdA==", "x": 1 }"""
      "@addProperty(xml('<test/>'),'x',1)",
      """{ "$content-type": "application/xml;charset=utf-8", "$content": "PHRlc3QgLz4=", "x": 1 }""" ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``addProperty binary/xml test cases``)>]
let ``Test addProperty with binary and xml`` expr expected = objTest expr expected

let ``addProperty case-conflicting test cases`` =
    [ "@addProperty(json('{\"a\": 1}'), 'A', 2)"
      "@addProperty(json('{\"A\": 1}'), 'a', 2)"
      "@addProperty(json('{\"a\": 1, \"A\": 5}'), 'A', 2)"
      "@addProperty(json('{\"A\": 1, \"a\": 5}'), 'a', 2)"
      "@addProperty(json('{\"aAa\": 1, \"AaA\": 2}'), 'aaa', 5)"
      "@addProperty(json('{\"aAa\": 1, \"AaA\": 2}'), 'AAA', 5)"
      "@addProperty(json('{\"AaA\": 1, \"aAa\": 2}'), 'aaa', 5)"
      "@addProperty(json('{\"AaA\": 1, \"aAa\": 2}'), 'AAA', 5)" ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``addProperty case-conflicting test cases``)>]
let ``Test addProperty case-conflicting`` expr =
    let parsed = trap <@ parseExpr (lexExpr expr) @>

    raisesWithOrTraceParsed parsed <@ evaluateParsed parsed @> (fun e ->
        let message = e.Message in <@ message.Contains("property to not exist in the object") @>)

let ``setProperty test cases`` =
    [ "@setProperty(json('{}'),'a','')", Ok """{ "a": "" }""" // set in empty object
      "@setProperty(json('{\"a\":1,\"b\":2,\"c\":3,\"d\":4}'),'c',3)", Ok """{ "a": 1, "b": 2, "d": 4, "c": 3 }""" // set existing key to existing value
      "@setProperty(json('{\"a\":1,\"b\":2,\"c\":3,\"d\":4}'),'c',999)", Ok """{ "a": 1, "b": 2, "d": 4, "c": 999 }""" // set existing key to new value
      "@setProperty(json('{\"a\":1,\"b\":2,\"d\":4}'),'c',3)", Ok """{ "a": 1, "b": 2, "d": 4, "c": 3 }""" // set new key to new value
      "@setProperty(json('{\"a\":1,\"b\":2,\"c\":3,\"d\":4}'),'',null)",
      Ok """{ "a": 1, "b": 2, "c": 3, "d": 4, "": null }""" // set empty key to null
      "@setProperty(json('{\"a\":1,\"b\":2,\"c\":3,\"d\":4}'),null,'')", Error "be of type string" // set null key
      "@setProperty(json('{}'),1,1)", Error "be of type string" // set integer key
      "@setProperty(json('{}'),false,1)", Error "be of type string" // set boolean key
      "@setProperty(json('{}'),json('[]'),1)", Error "be of type string" // set array key
      "@setProperty(json('{}'),json('[\"a\"]'),1)", Error "be of type string" // set array-of-string key
      "@setProperty(json('{}'),'value','a','b')", Error "expects three parameters" // too many parameters
      "@setProperty(json('{}'),'value')", Error "expects three parameters" // too few parameters
      "@setProperty(null,'x',1)", Error "be of type object" // null object
      "@setProperty(true,'x',1)", Error "be of type object" // boolean object
      "@setProperty(1,'x',1)", Error "be of type object" // integer object
      "@setProperty('abc','x',1)", Error "be of type object" // string object
      "@setProperty('{}','x',1)", Error "be of type object" // stringified object
      "@setProperty(createArray(1,2,3),'x',1)", Error "be of type object" ] // array object
    |> List.map TestCaseData

[<TestCaseSource(nameof ``setProperty test cases``)>]
let ``Test setProperty`` expr expected = objOrFailTest expr expected

let ``setProperty binary/xml test cases`` =
    // Note: These are wrapped in an outer object so that they don't get pretty-printed in the outputs of the logic
    // app overview. They are unchanged from the actual expressions which were tested.
    [ "@setProperty(json('{}'),'v',setProperty(binary('test'),'x',1))",
      """{ "v": { "$content-type": "application/octet-stream", "$content": "dGVzdA==", "x": 1 } }""" // set property on binary
      "@setProperty(json('{}'),'v',setProperty(xml('<test/>'),'x',1))",
      """{ "v": { "$content-type": "application/xml;charset=utf-8", "$content": "PHRlc3QgLz4=", "x": 1 } }""" ] // set property on xml
    |> List.map TestCaseData

[<TestCaseSource(nameof ``setProperty binary/xml test cases``)>]
let ``Test setProperty with binary and xml`` expr expected = objTest expr expected

let ``setProperty case-conflicting test cases`` =
    [ "@setProperty(json('{\"a\": 1}'), 'A', 2)", """{ "a": 2 }"""
      "@setProperty(json('{\"A\": 1}'), 'a', 2)", """{ "A": 2 }"""
      "@setProperty(json('{\"a\": 1, \"A\": 5}'), 'A', 2)", """{ "a": 1, "A": 2 }"""
      "@setProperty(json('{\"A\": 1, \"a\": 5}'), 'a', 2)", """{ "A": 1, "a": 2 }"""
      "@setProperty(json('{\"aAa\": 1, \"AaA\": 2}'), 'aaa', 5)", """{ "AaA": 2, "aAa": 5 }"""
      "@setProperty(json('{\"aAa\": 1, \"AaA\": 2}'), 'AAA', 5)", """{ "AaA": 2, "aAa": 5 }"""
      "@setProperty(json('{\"AaA\": 1, \"aAa\": 2}'), 'aaa', 5)", """{ "aAa": 2, "AaA": 5 }"""
      "@setProperty(json('{\"AaA\": 1, \"aAa\": 2}'), 'AAA', 5)", """{ "aAa": 2, "AaA": 5 }""" ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``setProperty case-conflicting test cases``)>]
let ``Test setProperty case-conflicting`` expr expected = objTest expr expected

let ``removeProperty case-conflicting test cases`` =
    [ "@removeProperty(json('{\"A\": 1}'), 'a')", """{}"""
      "@removeProperty(json('{\"a\": 1}'), 'A')", """{}"""
      "@removeProperty(json('{\"A\": 1, \"a\": 2}'), 'a')", """{ "A": 1 }"""
      "@removeProperty(json('{\"a\": 1, \"A\": 2}'), 'A')", """{ "a": 1 }"""
      "@removeProperty(json('{\"A\": 1, \"a\": 2}'), 'A')", """{ "a": 2 }"""
      "@removeProperty(json('{\"a\": 1, \"A\": 2}'), 'a')", """{ "A": 2 }"""
      "@removeProperty(json('{\"aAa\": 1, \"AaA\": 2}'), 'aaa')", """{ "AaA": 2 }"""
      "@removeProperty(json('{\"aAa\": 1, \"AaA\": 2}'), 'AAA')", """{ "AaA": 2 }"""
      "@removeProperty(json('{\"AaA\": 1, \"aAa\": 2}'), 'aaa')", """{ "aAa": 2 }"""
      "@removeProperty(json('{\"AaA\": 1, \"aAa\": 2}'), 'AAA')", """{ "aAa": 2 }""" ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``removeProperty case-conflicting test cases``)>]
let ``Test removeProperty case-conflicting`` expr expected = objTest expr expected

let ``xpath test cases`` =
    [ "@xpath('<r><a>this is a</a><b>second</b><c>number c</c></r>','/r/*')", Error "be an XML object"
      "@xpath(xml(json('{\"r\":{\"v\":[1,2,3,4,5]}}')),'sum(/r/v)')", Ok "15.0"
      "@xpath(xml(json('{\"r\":{\"v\":[1.0,2,3,4,5]}}')),'sum(/r/v)')", Ok "15.0"
      "@xpath(xml('<r><a>this is a</a><b>second</b><c>number c</c></r>'),'/r/b')",
      Ok
          """
          [
            {
              "$content-type": "application/xml;charset=utf-8",
              "$content": "PGI+c2Vjb25kPC9iPg=="
            }
          ]"""
      "@xpath(xml('<r><a>this is a</a><b>second</b><c>number c</c></r>'),'/r/*')",
      Ok
          """
          [
            {
              "$content-type": "application/xml;charset=utf-8",
              "$content": "PGE+dGhpcyBpcyBhPC9hPg=="
            },
            {
              "$content-type": "application/xml;charset=utf-8",
              "$content": "PGI+c2Vjb25kPC9iPg=="
            },
            {
              "$content-type": "application/xml;charset=utf-8",
              "$content": "PGM+bnVtYmVyIGM8L2M+"
            }
          ]"""
      "@xpath(xml('<r><a>this is a</a><b>second</b><c>number c</c></r>'),'/r/*/text()')",
      Ok """["this is a","second","number c"]""" ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``xpath test cases``)>]
let ``Test xpath`` expr expected = objOrFailTest expr expected
