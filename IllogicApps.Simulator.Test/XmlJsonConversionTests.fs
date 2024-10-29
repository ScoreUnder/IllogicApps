module IllogicApps.Simulator.Test.XmlJsonConversionTests

open NUnit.Framework
open Swensen.Unquote

open JsonUtil
open TestSimUtil

[<Test>]
let Test1 () =
    test <@ testExpressionEvaluation "@json(binary('[1,2,3]'))" |> jsonsEqual (jsonOfObject [| 1; 2; 3 |]) @>
