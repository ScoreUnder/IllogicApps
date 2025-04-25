module IllogicApps.Json.Test.RoundTripTests

open NUnit.Framework
open Swensen.Unquote

open IllogicApps.Json
open IllogicApps.Json.Conversions
open IllogicApps.Json.JsonParser

[<Test>]
let ``Round-tripping a empty string`` () =
    let s = String ""
    test <@ parse (stringOfJson s) = s @>

[<Test>]
let ``Round-tripping a string with special characters`` () =
    let s = String "\"Hello\nWorld\" = \x01\u3333"
    test <@ parse (stringOfJson s) = s @>

[<Test>]
let ``Round-tripping an empty array`` () =
    let s = createArray []
    test <@ parse (stringOfJson s) = s @>

[<Test>]
let ``Round-tripping an array with a single element`` () =
    let s = createArray [ String "Hello" ]
    test <@ parse (stringOfJson s) = s @>

[<Test>]
let ``Round-tripping an empty object`` () =
    let s = createObject []
    test <@ parse (stringOfJson s) = s @>

[<Test>]
let ``Round-tripping an object with a single key-value pair`` () =
    let s = createObject [ "key", String "value" ]
    test <@ parse (stringOfJson s) = s @>

[<TestCase(true)>]
[<TestCase(false)>]
let ``Round-tripping a boolean`` value =
    let b = Boolean value
    test <@ parse (stringOfJson b) = b @>

[<Test>]
let ``Round-tripping null`` () =
    test <@ parse (stringOfJson Null) = Null @>

let integerTestCases =
    [ TestCaseData(0L)
      TestCaseData(1L)
      TestCaseData(-1L)
      TestCaseData(System.Int64.MinValue)
      TestCaseData(System.Int64.MaxValue) ]

[<TestCaseSource("integerTestCases")>]
let ``Round-tripping an integer`` value =
    let i = Integer value
    test <@ parse (stringOfJson i) = i @>

[<TestCase(1.1)>]
[<TestCase(-1.1)>]
[<TestCase(0.1)>]
[<TestCase(-0.1)>]
[<TestCase(System.Double.MinValue)>]
[<TestCase(System.Double.MaxValue)>]
[<TestCase(System.Double.Epsilon)>]
[<TestCase(-System.Double.Epsilon)>]
let ``Round-tripping a float`` value =
    let f = Float value
    test <@ parse (stringOfJson f) = f @>

let floatCloseToRoundNumberCases =
    [ TestCaseData(System.Double.BitIncrement 1.0)
      TestCaseData(-System.Double.BitIncrement 1.0)
      TestCaseData(System.Double.BitDecrement 1.0)
      TestCaseData(-System.Double.BitDecrement 1.0)
      TestCaseData(System.Double.BitIncrement 1e30)
      TestCaseData(-System.Double.BitIncrement 1e30)
      TestCaseData(System.Double.BitDecrement 1e30)
      TestCaseData(-System.Double.BitDecrement 1e30)
      TestCaseData(System.Double.BitIncrement 1e-30)
      TestCaseData(-System.Double.BitIncrement 1e-30)
      TestCaseData(System.Double.BitDecrement 1e-30)
      TestCaseData(-System.Double.BitDecrement 1e-30) ]

[<TestCaseSource("floatCloseToRoundNumberCases")>]
let ``Edge case: round-tripping a float very close to a round number`` n =
    let f = Float n
    test <@ parse (stringOfJson f) = f @>
