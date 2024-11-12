module IllogicApps.Simulator.Test.StringFunctionsTests

open NUnit.Framework
open TestSimUtil

let ``length test cases`` =
    [ "@length('123456')", Ok "6" // length of string
      "@length('')", Ok "0" // length of empty string
      "@length(createArray(1,2,3,4,5))", Ok "5" // length of array
      "@length(createArray('aa','bb','cc'))", Ok "3" // length of array of strings
      "@length(json('[]'))", Ok "0" // length of empty array
      "@length(json('{}'))", Error "be an array or a string" // length of object
      "@length(null)", Error "be an array or a string" // length of null
      "@length(true)", Error "be an array or a string" // length of boolean
      "@length(99)", Error "be an array or a string" // length of integer
      "@length(xml('<root/>'))", Error "be an array or a string" // length of xml
      "@length(binary('abc'))", Error "be an array or a string" ] // length of binary
    |> List.map TestCaseData

[<TestCaseSource(nameof ``length test cases``)>]
let ``Test length`` expr expected = objOrFailTest expr expected

let ``replace test cases`` =
    [ "@replace('xxxxx','x','y')", Ok "yyyyy" // replace replaces all
      "@replace('xxxxx','x','')", Ok "" // replace can replace with empty
      "@replace('xxxxx','','y')", Error "String cannot be of zero length. (Parameter 'oldValue')" // replace old value cannot be empty
      "@replace('abcdef','e','z')", Ok "abcdzf" // replace single character
      "@replace('abcdef','cd','z')", Ok "abzef" // replace multiple characters
      "@replace('abcdef','z','o')", Ok "abcdef" // replace non-existent
      "@replace('abcdef','.','z')", Ok "abcdef" ] // making sure it's not regex
    |> List.map TestCaseData

[<TestCaseSource(nameof ``replace test cases``)>]
let ``Test replace`` expr expected = stringOrFailTest expr expected

[<TestCase("@slice('abcdef',0)", "abcdef")>] // slice whole string
[<TestCase("@slice('abcdef',0,99)", "abcdef")>] // slice whole string OOB end
[<TestCase("@slice('abcdef',3)", "def")>] // slice part of string
[<TestCase("@slice('abcdef',3,4)", "d")>] // slice part of string with end
[<TestCase("@slice('abcdef',39,99)", "")>] // slice both OOB
[<TestCase("@slice('abcdef',39,10)", "")>] // slice both OOB, end < start
[<TestCase("@slice('abcdef',-3)", "def")>] // slice from end
[<TestCase("@slice('abcdef',-3,1)", "")>] // slice from end, end < start
[<TestCase("@slice('abcdef',-3,-1)", "de")>] // slice from end, end from end
[<TestCase("@slice('abcdef',-33)", "abcdef")>] // slice from end OOB
[<TestCase("@slice('abcdef',-33,-44)", "")>] // slice from end OOB, end from end OOB, end < start
[<TestCase("@slice('abcdef',-1,-2)", "")>] // slice from end, end from end, end < start
let ``Test slice`` (expr: string, expected: string) = stringTest expr expected

let ``split test cases`` =
    [ "@split('abc,def',',')", Ok """[ "abc", "def" ]""" // split on comma
      "@split('abc,def',';')", Ok """[ "abc,def" ]""" // split on non-existent
      "@split('abc,def,ghi,jkl',',')", Ok """[ "abc", "def", "ghi", "jkl" ]""" // split on multiple
      "@split('abc,def,',',')", Ok """[ "abc", "def", "" ]""" // split with trailing
      "@split(',abc,def',',')", Ok """[ "", "abc", "def" ]""" // split with leading
      "@split(',abc,def,',',')", Ok """[ "", "abc", "def", "" ]""" // split with leading and trailing
      "@split('abc,,,def',',')", Ok """[ "abc", "", "", "def" ]""" // split with empty
      "@split(',,,,',',')", Ok """[ "", "", "", "", "" ]""" // split with all empty
      "@split('abc,def','.')", Ok """[ "abc,def" ]""" // ensure not regex
      "@split('abcSPdefSPghi','SP')", Ok """[ "abc", "def", "ghi" ]""" // split on string
      "@split('abc,def','')", Ok """[ "abc,def" ]""" // split on empty
      "@split('',',')", Ok """[ "" ]""" // split empty string
      "@split('','')", Ok """[ "" ]""" // split empty string on empty
      "@split('abc1def', 1)", Error "be of type string" // split on integer
      "@split('abctruedef',true)", Error "be of type string" // split on boolean
      "@split('abcnulldef',null)", Error "be of type string" // split on null
      "@split('abc,,,def',createArray(',','e'))", Error "be of type string" ] // split on array
    |> List.map TestCaseData

[<TestCaseSource(nameof ``split test cases``)>]
let ``Test split`` expr expected = objOrFailTest expr expected

let ``substring test cases`` =
    let rangeError =
        Error
            "out of range: 'start index' and 'length' must be non-negative integers and their sum must be no larger than the length of the string"

    [ "@substring('abcdef',0)", Ok "abcdef"
      "@substring('abcdef',0,99)", rangeError
      "@substring('abcdef',3)", Ok "def"
      "@substring('abcdef',3,1)", Ok "d"
      "@substring('abcdef',39,99)", rangeError
      "@substring('abcdef',39,-10)", rangeError
      "@substring('abcdef',-3)", rangeError
      "@substring('abcdef',-1,2)", rangeError
      "@substring('abcdef',-3,-1)", rangeError
      "@substring('abcdef',-33)", rangeError
      "@substring('abcdef',-33,-44)", rangeError ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``substring test cases``)>]
let ``Test substring`` expr expected = stringOrFailTest expr expected
