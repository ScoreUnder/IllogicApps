module IllogicApps.Expression.Execution.Test.CollectionFunctionsTests

open NUnit.Framework
open TestSimUtil
open IllogicApps.Json
open IllogicApps.Json.Conversions

let ``empty test cases`` =
    [ "@empty('')", Ok true // empty string
      "@empty('       ')", Ok false // whitespace string
      "@empty(json('[]'))", Ok true // empty array
      "@empty(json('[[]]'))", Ok false // array of empty array
      "@empty(json('{}'))", Ok true // empty object
      "@empty(json('{\"a\":\"b\"}'))", Ok false // object with key-value
      "@empty(json('{\"\":{}}'))", Ok false // object with empty key and empty object value
      "@empty(json('[\"\"]'))", Ok false // array with empty string
      "@empty(1)", Error "be an object, an array or a string" // integer
      "@empty(0)", Error "be an object, an array or a string" // integer 0
      "@empty(false)", Error "be an object, an array or a string" // false
      "@empty(true)", Error "be an object, an array or a string" // true
      "@empty(null)", Ok true // null
      "@empty(0.0)", Error "be an object, an array or a string" // float 0
      "@empty()", Error "expects one parameter" // no parameter
      "@empty('','')", Error "expects one parameter" ] // too many parameters
    |> List.map TestCaseData

[<TestCaseSource(nameof ``empty test cases``)>]
let ``Test empty`` expr expected = boolOrFailTest expr expected

let ``first test cases`` =
    let typeError = Error "be an array or a string"
    let arityError = Error "expects one parameter"

    // Using setProperty to make the test results more readable in the Logic Apps Overview
    [ "@setProperty(json('{}'),'value',first())", arityError // no parameter
      "@setProperty(json('{}'),'value',first('',''))", arityError // too many parameters
      "@setProperty(json('{}'),'value',first(''))", Ok """{ "value": "" }""" // empty string
      "@first('abc')", Ok "\"a\"" // string
      "@setProperty(json('{}'),'value',first(json('[]')))", Ok """{ "value": null }""" // empty array
      "@setProperty(json('{}'),'value',first(json('[\"a\",\"b\",\"c\"]')))", Ok """{ "value": "a" }""" // array of strings
      "@setProperty(json('{}'),'value',first(json('{}')))", typeError // empty object
      "@setProperty(json('{}'),'value',first(json('{\"a\":\"b\",\"c\":\"d\"}')))", typeError // object with entries
      "@setProperty(json('{}'),'value',first(1))", typeError // integer
      "@setProperty(json('{}'),'value',first(true))", typeError // boolean
      "@setProperty(json('{}'),'value',first(null))", typeError ] // null
    |> List.map TestCaseData

[<TestCaseSource(nameof ``first test cases``)>]
let ``Test first`` expr expected = objOrFailTest expr expected

let ``last test cases`` =
    let typeError = Error "be an array or a string"
    let arityError = Error "expects one parameter"

    [ "@setProperty(json('{}'),'value',last())", arityError // no parameter
      "@setProperty(json('{}'),'value',last('',''))", arityError // too many parameters
      "@setProperty(json('{}'),'value',last(''))", Ok """{ "value": "" }""" // empty string
      "@last('abc')", Ok "\"c\"" // string
      "@setProperty(json('{}'),'value',last(json('[]')))", Ok """{ "value": null }""" // empty array
      "@setProperty(json('{}'),'value',last(json('[\"a\",\"b\",\"c\"]')))", Ok """{ "value": "c" }""" // array of strings
      "@setProperty(json('{}'),'value',last(json('{}')))", typeError // empty object
      "@setProperty(json('{}'),'value',last(json('{\"a\":\"b\",\"c\":\"d\"}')))", typeError // object with entries
      "@setProperty(json('{}'),'value',last(1))", typeError // integer
      "@setProperty(json('{}'),'value',last(true))", typeError // boolean
      "@setProperty(json('{}'),'value',last(null))", typeError ] // null
    |> List.map TestCaseData

[<TestCaseSource(nameof ``last test cases``)>]
let ``Test last`` expr expected = objOrFailTest expr expected

let ``join test cases`` =
    [ "@join(array(''))", Error "expects two parameters" // no separator
      "@join(array(''),'','')", Error "expects two parameters" // too many parameters
      "@join(createArray('','hello','','world','','abc','def'), 'XYZ')", Ok "XYZhelloXYZXYZworldXYZXYZabcXYZdef" // string separator
      "@join(array(''), 'XYZ')", Ok "" // single-element array
      "@join(createArray('','hello','','world','','abc','def'), '')", Ok "helloworldabcdef" // empty separator
      "@join(createArray('','hello','','world','','abc','def'), null)", Error "be of type string" // null separator
      "@join('asdf','x')", Error "be an array" // string
      "@join(json('{}'),'x')", Error "be an array" // object
      "@join(json('[]'),'x')", Ok "" // empty array
      "@join(json('{\"a\":\"b\",\"c\":\"d\"}'),'x')", Error "be an array" // object with entries
      "@join(createArray('',null,''),'x')", Ok "xx" // array with null
      "@join(createArray('',1,''),'x')", Ok "x1x" // array with integer
      "@join(createArray('',true,''),'x')", Ok "xTruex" // array with boolean
      "@join(createArray('a',createArray('b','c'),'d'),'x')", Ok """ax["b","c"]xd""" // array with array
      "@join(createArray(binary('test'),xml('<!-- lol --><ing/>')),'x')", Ok """testx<!-- lol --><ing />""" ] // array with binary and xml
    |> List.map TestCaseData

[<TestCaseSource(nameof ``join test cases``)>]
let ``Test join`` expr expected = stringOrFailTest expr expected

let ``intersection test cases`` =
    [ "@intersection(createArray(5,4,3,2,1),createArray('a','b','c','d','e'))", Ok emptyArray
      "@intersection(createArray(5,4,3,2,1),createArray('a','b',3,5,1))",
      Ok <| createArray [ Integer 5; Integer 3; Integer 1 ]
      "@intersection(createArray('a','b',3,5,1),createArray(5,4,3,2,1))",
      Ok <| createArray [ Integer 3; Integer 5; Integer 1 ]
      "@intersection(createArray(1,2,3,4,4,5),createArray(3,4,5))",
      Ok <| createArray [ Integer 3; Integer 4; Integer 5 ]
      "@intersection(createArray(1,2,3,4,4,5),createArray(3,4,4,5))",
      Ok <| createArray [ Integer 3; Integer 4; Integer 5 ]
      "@intersection(createArray(1,2,3,4,5),createArray(3,4,4,5))",
      Ok <| createArray [ Integer 3; Integer 4; Integer 5 ]
      "@intersection(createArray(3,4,5),createArray(1,2,3,4,4,5))",
      Ok <| createArray [ Integer 3; Integer 4; Integer 5 ]
      "@intersection(createArray(3,4,4,5),createArray(1,2,3,4,4,5))",
      Ok <| createArray [ Integer 3; Integer 4; Integer 5 ]
      "@intersection(createArray(3,4,4,5),createArray(1,2,3,4,5))",
      Ok <| createArray [ Integer 3; Integer 4; Integer 5 ]
      "@intersection(json('{\"spam\":3,\"food\":\"eggs\"}'),createArray('spam','eggs'))",
      Error "expects parameters of same type"
      "@intersection(createArray('spam','eggs'),json('{\"spam\":3,\"food\":\"eggs\"}'))",
      Error "expects parameters of same type"
      "@intersection(json('{\"spam\":\"eggs\",\"thing\":3}'),json('{\"spam\":3,\"food\":\"eggs\"}'))", Ok emptyObject
      "@intersection(json('{\"spam\":\"eggs\",\"food\":\"bard\"}'),json('{\"spam\":3,\"food\":\"eggs\"}'))",
      Ok emptyObject
      "@intersection(json('{\"spam\":3,\"food\":\"eggs\"}'),json('{\"spam\":\"eggs\",\"food\":\"bard\"}'))",
      Ok emptyObject
      "@intersection(json('{\"spam\":\"eggs\",\"food\":\"fungus\"}'),json('{\"spam\":\"eggs\",\"food\":\"plant\"}'))",
      Ok <| createObject [ "spam", String "eggs" ]
      "@intersection(json('{\"spam\":\"eggs\",\"food\":[1,2,3]}'),json('{\"spam\":\"eggs\",\"food\":[5,4,1,-1]}'))",
      Ok <| createObject [ "spam", String "eggs" ]
      "@intersection(json('{\"spam\":\"eggs\",\"food\":[5,4,1,-1]}'),json('{\"spam\":\"eggs\",\"food\":[5,4,1,-1]}'))",
      Ok
      <| createObject
          [ "spam", String "eggs"
            "food", createArray [ Integer 5; Integer 4; Integer 1; Integer -1 ] ]
      "@intersection(json('{\"spam\":\"eggs\",\"food\":[5,4,1,-1]}'),json('{\"spam\":\"eggs\",\"food\":[1,2,3]}'))",
      Ok <| createObject [ "spam", String "eggs" ]
      "@intersection(createArray(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30),createArray(0,3,6,9,12,15,18,21,24,27,30),createArray(0,5,10,15,20,25,30))",
      Ok <| createArray [ Integer 0; Integer 30 ]
      "@intersection(createArray(30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0),createArray(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30),createArray(0,3,6,9,12,15,18,21,24,27,30),createArray(0,5,10,15,20,25,30))",
      Ok <| createArray [ Integer 30; Integer 0 ]
      "@intersection(createArray(5,4,5,5,4,5,4,4,5,4,5,5,4),json('[]'))", Ok emptyArray
      "@intersection(json('[]'),createArray(5,4,5,5,4,5,4,4,5,4,5,5,4))", Ok emptyArray
      "@intersection(createArray(5,4,5,5,4,5,4,4,5,4,5,5,4),createArray(4,4,4,4,5,4,5,5,5))",
      Ok <| createArray [ Integer 5; Integer 4 ]
      "@intersection(createArray(4,4,4,4,5,4,5,5,5),createArray(5,4,5,5,4,5,4,4,5,4,5,5,4))",
      Ok <| createArray [ Integer 4; Integer 5 ] ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``intersection test cases``)>]
let ``Test intersection`` expr expected = jsonOrFailTest expr expected

let ``union test cases`` =
    [ "@union(createArray(5,4,3,2,1),createArray('a','b','c','d','e'))",
      Ok
      <| createArray
          [ Integer 5
            Integer 4
            Integer 3
            Integer 2
            Integer 1
            String "a"
            String "b"
            String "c"
            String "d"
            String "e" ]
      "@union(createArray(5,4,3,2,1),createArray('a','b',3,5,1))",
      Ok
      <| createArray
          [ Integer 5
            Integer 4
            Integer 3
            Integer 2
            Integer 1
            String "a"
            String "b" ]
      "@union(createArray('a','b',3,5,1),createArray(5,4,3,2,1))",
      Ok
      <| createArray
          [ String "a"
            String "b"
            Integer 3
            Integer 5
            Integer 1
            Integer 4
            Integer 2 ]
      "@union(createArray(1,2,3,4,4,5),createArray(3,4,5))",
      Ok <| createArray [ Integer 1; Integer 2; Integer 3; Integer 4; Integer 5 ]
      "@union(createArray(1,2,3,4,4,5),createArray(3,4,4,5))",
      Ok <| createArray [ Integer 1; Integer 2; Integer 3; Integer 4; Integer 5 ]
      "@union(createArray(1,2,3,4,5),createArray(3,4,4,5))",
      Ok <| createArray [ Integer 1; Integer 2; Integer 3; Integer 4; Integer 5 ]
      "@union(createArray(3,4,5),createArray(1,2,3,4,4,5))",
      Ok <| createArray [ Integer 3; Integer 4; Integer 5; Integer 1; Integer 2 ]
      "@union(createArray(3,4,4,5),createArray(1,2,3,4,4,5))",
      Ok <| createArray [ Integer 3; Integer 4; Integer 5; Integer 1; Integer 2 ]
      "@union(createArray(3,4,4,5),createArray(1,2,3,4,5))",
      Ok <| createArray [ Integer 3; Integer 4; Integer 5; Integer 1; Integer 2 ]
      "@union(json('{\"spam\":3,\"food\":\"eggs\"}'),createArray('spam','eggs'))",
      Error "expects parameters of same type"
      "@union(createArray('spam','eggs'),json('{\"spam\":3,\"food\":\"eggs\"}'))",
      Error "expects parameters of same type"
      "@union(json('{\"spam\":\"eggs\",\"thing\":3}'),json('{\"spam\":3,\"food\":\"eggs\"}'))",
      Ok
      <| createObject [ "spam", Integer 3; "thing", Integer 3; "food", String "eggs" ]
      "@union(json('{\"spam\":\"eggs\",\"food\":\"bard\"}'),json('{\"spam\":3,\"food\":\"eggs\"}'))",
      Ok <| createObject [ "spam", Integer 3; "food", String "eggs" ]
      "@union(json('{\"spam\":3,\"food\":\"eggs\"}'),json('{\"spam\":\"eggs\",\"food\":\"bard\"}'))",
      Ok <| createObject [ "spam", String "eggs"; "food", String "bard" ]
      "@union(json('{\"spam\":\"eggs\",\"food\":\"fungus\"}'),json('{\"spam\":\"eggs\",\"food\":\"plant\"}'))",
      Ok <| createObject [ "spam", String "eggs"; "food", String "plant" ]
      "@union(json('{\"spam\":\"eggs\",\"food\":[1,2,3]}'),json('{\"spam\":\"eggs\",\"food\":[5,4,1,-1]}'))",
      Ok
      <| createObject
          [ "spam", String "eggs"
            "food", createArray [ Integer 5; Integer 4; Integer 1; Integer -1 ] ]
      "@union(json('{\"spam\":\"eggs\",\"food\":[5,4,1,-1]}'),json('{\"spam\":\"eggs\",\"food\":[5,4,1,-1]}'))",
      Ok
      <| createObject
          [ "spam", String "eggs"
            "food", createArray [ Integer 5; Integer 4; Integer 1; Integer -1 ] ]
      "@union(json('{\"spam\":\"eggs\",\"food\":[5,4,1,-1]}'),json('{\"spam\":\"eggs\",\"food\":[1,2,3]}'))",
      Ok
      <| createObject
          [ "spam", String "eggs"
            "food", createArray [ Integer 1; Integer 2; Integer 3 ] ]
      "@union(createArray(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30),createArray(0,3,6,9,12,15,18,21,24,27,30),createArray(0,5,10,15,20,25,30))",
      Ok
      <| createArray (
          List.map
              Integer
              [ 0
                2
                4
                6
                8
                10
                12
                14
                16
                18
                20
                22
                24
                26
                28
                30
                3
                9
                15
                21
                27
                5
                25 ]
      )
      "@union(createArray(30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0),createArray(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30),createArray(0,3,6,9,12,15,18,21,24,27,30),createArray(0,5,10,15,20,25,30))",
      Ok <| createArray (List.init 31 (fun i -> Integer(30L - int64 i)))
      "@union(createArray(5,4,5,5,4,5,4,4,5,4,5,5,4),json('[]'))", Ok <| createArray [ Integer 5; Integer 4 ]
      "@union(json('[]'),createArray(5,4,5,5,4,5,4,4,5,4,5,5,4))", Ok <| createArray [ Integer 5; Integer 4 ]
      "@union(createArray(5,4,5,5,4,5,4,4,5,4,5,5,4),createArray(4,4,4,4,5,4,5,5,5))",
      Ok <| createArray [ Integer 5; Integer 4 ]
      "@union(createArray(4,4,4,4,5,4,5,5,5),createArray(5,4,5,5,4,5,4,4,5,4,5,5,4))",
      Ok <| createArray [ Integer 4; Integer 5 ] ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``union test cases``)>]
let ``Test union`` expr expected = jsonOrFailTest expr expected
