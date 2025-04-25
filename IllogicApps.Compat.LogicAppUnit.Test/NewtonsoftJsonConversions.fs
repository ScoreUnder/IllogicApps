module IllogicApps.Compat.LogicAppUnit.Test

open NUnit.Framework
open Newtonsoft.Json.Linq
open Swensen.Unquote

open IllogicApps.Json
open NewtonsoftJsonConversions

let ``JSON round-trip test cases`` =
    [ """{"null": null, "int": 123, "float": 123.0, "bool": false, "array": [1,2,3], "object": {}}""" ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``JSON round-trip test cases``)>]
let ``Test Illogic-to-Newtonsoft conversion`` expr =
    let illogic = JsonParser.parse expr
    let newtonsoft = newtonsoftJsonOfIllogicJson illogic
    let newtonsoftString = newtonsoft.ToString()
    let illogicOfString = JsonParser.parse newtonsoftString
    test <@ illogicOfString = illogic @>

[<TestCaseSource(nameof ``JSON round-trip test cases``)>]
let ``Test Newtonsoft-to-Illogic conversion`` expr =
    let newtonsoft = JToken.Parse expr
    let illogic = illogicJsonOfNewtonsoftJson newtonsoft
    let illogicString = illogic.ToString()
    let newtonsoftOfString = JToken.Parse illogicString
    test <@ JToken.DeepEquals(newtonsoftOfString, newtonsoft) @>
