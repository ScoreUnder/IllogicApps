module IllogicApps.Simulator.Test.LogicTests

open NUnit.Framework
open IllogicApps.Json
open TestSimUtil

let ``Test cases for short-circuiting "and"`` =
    [ "@and(false,int('blkadokg'))", Ok false
      "@and(int('blkadokg'),false)", Error "Could not parse blkadokg as int"
      "@and(false,faweorawoekjf())", Ok false
      "@and(and(and(true,true),false),nope())", Ok false
      "@and(and(and(true,false),true),nope())", Ok false
      "@and(and(and(false,true),true),nope())", Ok false
      "@and(and(and(true,true),true),nope())", Error "The template function 'nope' is not defined or not valid" ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``Test cases for short-circuiting "and"``)>]
let ``Test short-circuiting "and"`` (expr: string, expected: Result<bool, string>) =
    match expected with
    | Ok expected -> testOrTrace expr <@ Boolean expected = testExpressionEvaluation expr @>
    | Error expected ->
        raisesWithOrTrace<System.Exception> expr <@ testExpressionEvaluation expr @> (fun e ->
            let message = e.Message in <@ message.Contains(expected) @>)

let ``Test cases for short-circuiting "or"`` =
    [ "@or(true,int('blkadokg'))", Ok true
      "@or(int('blkadokg'),true)", Error "Could not parse blkadokg as int"
      "@or(true,faweorawoekjf())", Ok true
      "@or(or(or(false,false),true),nope())", Ok true
      "@or(or(or(false,true),false),nope())", Ok true
      "@or(or(or(true,false),false),nope())", Ok true
      "@or(or(or(false,false),false),nope())", Error "The template function 'nope' is not defined or not valid" ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``Test cases for short-circuiting "or"``)>]
let ``Test short-circuiting "or"`` (expr: string, expected: Result<bool, string>) =
    match expected with
    | Ok expected -> testOrTrace expr <@ Boolean expected = testExpressionEvaluation expr @>
    | Error expected ->
        raisesWithOrTrace<System.Exception> expr <@ testExpressionEvaluation expr @> (fun e ->
            let message = e.Message in <@ message.Contains(expected) @>)

let ``Test cases for short-circuiting "if"`` =
    [ "@if(true,1,throwMeAnError())", Ok 1
      "@if(false,throwMeAnError(),2)", Ok 2
      "@if(true,throwMeAnError(),2)", Error "The template function 'throwMeAnError' is not defined or not valid"
      "@if(false,1,throwMeAnError())", Error "The template function 'throwMeAnError' is not defined or not valid"
      "@if(if(true,false,true),if(true,throwMeAnError(),throwAnotherError()),if(true,secretThirdError(),4))", Error "The template function 'secretThirdError' is not defined or not valid"
      "@if(if(false,false,true),if(false,throwMeAnError(),throwAnotherError()),if(false,secretThirdError(),4))", Error "The template function 'throwAnotherError' is not defined or not valid" ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``Test cases for short-circuiting "if"``)>]
let ``Test short-circuiting "if"`` (expr: string, expected: Result<int, string>) =
    match expected with
    | Ok expected -> testOrTrace expr <@ Integer expected = testExpressionEvaluation expr @>
    | Error expected ->
        raisesWithOrTrace<System.Exception> expr <@ testExpressionEvaluation expr @> (fun e ->
            let message = e.Message in <@ message.Contains(expected) @>)

[<TestCase("@and(true,true)", true)>]
[<TestCase("@and(false,true)", false)>]
[<TestCase("@and(true,false)", false)>]
[<TestCase("@and(false,false)", false)>]
[<TestCase("@and(and(false,true),and(true,true))", false)>]
[<TestCase("@and(and(true,false),and(true,true))", false)>]
[<TestCase("@and(and(true,true),and(false,true))", false)>]
[<TestCase("@and(and(true,true),and(true,false))", false)>]
[<TestCase("@and(and(true,true),and(true,true))", true)>]
let ``Test basic "and"`` expr expected =
    testOrTrace expr <@ Boolean expected = testExpressionEvaluation expr @>

[<TestCase("@or(true,true)", true)>]
[<TestCase("@or(false,true)", true)>]
[<TestCase("@or(true,false)", true)>]
[<TestCase("@or(false,false)", false)>]
[<TestCase("@or(or(false,true),or(true,true))", true)>]
[<TestCase("@or(or(true,false),or(true,true))", true)>]
[<TestCase("@or(or(true,true),or(false,true))", true)>]
[<TestCase("@or(or(true,true),or(true,false))", true)>]
[<TestCase("@or(or(false,false),or(false,false))", false)>]
let ``Test basic "or"`` expr expected =
    testOrTrace expr <@ Boolean expected = testExpressionEvaluation expr @>

[<TestCase("@if(true,1,2)", 1)>]
[<TestCase("@if(false,1,2)", 2)>]
[<TestCase("@if(if(false,false,true),if(true,1,2),if(true,3,4))", 1)>]
[<TestCase("@if(if(false,false,true),if(false,1,2),if(false,3,4))", 2)>]
[<TestCase("@if(if(true,false,true),if(true,1,2),if(true,3,4))", 3)>]
[<TestCase("@if(if(true,false,true),if(false,1,2),if(false,3,4))", 4)>]
let ``Test basic "if"`` expr expected =
    testOrTrace expr <@ Integer expected = testExpressionEvaluation expr @>

[<TestCase("@and(true)", true)>]
[<TestCase("@and(false)", false)>]
[<TestCase("@and(true,true,true,true,true,true,true,true,true)", true)>]
[<TestCase("@and(true,true,true,true,true,true,true,true,false)", false)>]
[<TestCase("@and(true,true,true,false,true,true,true,true,true)", false)>]
[<TestCase("@and(false,true,true,true,true,true,true,true,true)", false)>]
let ``Test multiple argument "and"`` expr expected =
    testOrTrace expr <@ Boolean expected = testExpressionEvaluation expr @>

[<TestCase("@or(true)", true)>]
[<TestCase("@or(false)", false)>]
[<TestCase("@or(false,false,false,false,false,false,false,false,false)", false)>]
[<TestCase("@or(false,false,false,false,false,false,false,false,true)", true)>]
[<TestCase("@or(false,false,false,true,false,false,false,false,false)", true)>]
[<TestCase("@or(true,false,false,false,false,false,false,false,false)", true)>]
let ``Test multiple argument "or"`` expr expected =
    testOrTrace expr <@ Boolean expected = testExpressionEvaluation expr @>

[<TestCase("@or()")>]
[<TestCase("@and()")>]
let ``Test empty argument "or" and "and"`` expr =
    raisesWithOrTrace<System.Exception> expr <@ testExpressionEvaluation expr @> (fun e ->
        let message = e.Message in <@ message.Contains("Expected at least one argument") @>)
