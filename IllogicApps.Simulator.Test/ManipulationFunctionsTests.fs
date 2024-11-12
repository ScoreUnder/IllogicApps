module IllogicApps.Simulator.Test.ManipulationFunctionsTests

open NUnit.Framework
open TestSimUtil

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
      "@setProperty(json('{}'),'value')", Error "expects three parameters" ] // too few parameters
    |> List.map TestCaseData

[<TestCaseSource(nameof ``setProperty test cases``)>]
let ``Test setProperty`` expr expected = objOrFailTest expr expected
