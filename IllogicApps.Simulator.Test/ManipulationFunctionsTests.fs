module IllogicApps.Simulator.Test.ManipulationFunctionsTests

open NUnit.Framework
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
