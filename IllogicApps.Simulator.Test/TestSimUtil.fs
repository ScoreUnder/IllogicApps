module IllogicApps.Simulator.Test.TestSimUtil

open IllogicApps.Core
open IllogicApps.Simulator

let makeSimulator () = Foq.Mock<SimulatorContext>().Create()

let testExpressionEvaluation expr =
    LanguageEvaluator.evaluateIfNecessary (makeSimulator ()) expr
