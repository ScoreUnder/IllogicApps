module IllogicApps.Expression.Execution.Test.CollectionFunctionsTests

open NUnit.Framework
open TestSimUtil

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
