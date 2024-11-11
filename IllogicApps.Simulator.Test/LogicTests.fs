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
      "@and(and(and(true,true),true),nope())", Error "Function nope not found" ]
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
      "@or(or(or(false,false),false),nope())", Error "Function nope not found" ]
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
      "@if(true,throwMeAnError(),2)", Error "Function throwMeAnError not found"
      "@if(false,1,throwMeAnError())", Error "Function throwMeAnError not found"
      "@if(if(true,false,true),if(true,throwMeAnError(),throwAnotherError()),if(true,secretThirdError(),4))", Error "Function secretThirdError not found"
      "@if(if(false,false,true),if(false,throwMeAnError(),throwAnotherError()),if(false,secretThirdError(),4))", Error "Function throwAnotherError not found" ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``Test cases for short-circuiting "if"``)>]
let ``Test short-circuiting "if"`` (expr: string, expected: Result<int, string>) =
    match expected with
    | Ok expected -> testOrTrace expr <@ Integer expected = testExpressionEvaluation expr @>
    | Error expected ->
        raisesWithOrTrace<System.Exception> expr <@ testExpressionEvaluation expr @> (fun e ->
            let message = e.Message in <@ message.Contains(expected) @>)

[<TestCase("@and(and(false,true),and(true,true))", false)>]
[<TestCase("@and(and(true,false),and(true,true))", false)>]
[<TestCase("@and(and(true,true),and(false,true))", false)>]
[<TestCase("@and(and(true,true),and(true,false))", false)>]
[<TestCase("@and(and(true,true),and(true,true))", true)>]
let ``Test basic "and"`` expr expected =
    testOrTrace expr <@ Boolean expected = testExpressionEvaluation expr @>

[<TestCase("@or(or(false,true),or(true,true))", true)>]
[<TestCase("@or(or(true,false),or(true,true))", true)>]
[<TestCase("@or(or(true,true),or(false,true))", true)>]
[<TestCase("@or(or(true,true),or(true,false))", true)>]
[<TestCase("@or(or(false,false),or(false,false))", false)>]
let ``Test basic "or"`` expr expected =
    testOrTrace expr <@ Boolean expected = testExpressionEvaluation expr @>

[<TestCase("@if(true,1,2)", 1)>]
[<TestCase("@if(false,1,2)", 2)>]
let ``Test basic "if"`` expr expected =
    testOrTrace expr <@ Integer expected = testExpressionEvaluation expr @>
