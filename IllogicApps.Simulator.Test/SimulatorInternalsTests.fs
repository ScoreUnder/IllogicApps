module IllogicApps.Simulator.Test.SimulatorInternalsTests

open IllogicApps.Json
open NUnit.Framework
open Swensen.Unquote

open IllogicApps.Simulator

let makeSimulator () =
    Simulator.CreateUntriggered(SimulatorCreationOptions.dummy)

[<Test>]
let ``Test variable names are case-insensitive`` () =
    let sim = makeSimulator ()

    let value = String "TestValue"

    sim.SetVariable "dummy" (String "Dummy")
    sim.SetVariable "TestVar" value
    sim.SetVariable "otherVar" (String "Other")

    test <@ sim.GetVariable("testvar") = Some value @>

// TODO: test that action names in GetActionResult are case-insensitive
// (or test via workflow trace tests)
