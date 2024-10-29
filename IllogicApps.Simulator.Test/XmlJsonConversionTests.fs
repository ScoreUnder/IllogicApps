module IllogicApps.Simulator.Test.XmlJsonConversionTests

open NUnit.Framework
open Swensen.Unquote

open IllogicApps.Core.JsonUtil
open TestSimUtil

[<Test>]
let Test1 () =
    test <@ testExpressionEvaluation "@json(binary('[1,2,3]'))" |> jsonsEqual (jsonOf [| jsonOf 1L; jsonOf 2L; jsonOf 3L |]) @>
