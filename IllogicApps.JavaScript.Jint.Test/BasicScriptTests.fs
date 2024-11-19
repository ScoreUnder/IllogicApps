module IllogicApps.JavaScript.Jint.Test

open IllogicApps.Core.CompletedStepTypes
open IllogicApps.Json
open NUnit.Framework
open Swensen.Unquote

open IllogicApps.Core
open IllogicApps.Core.ExternalServiceTypes
open IllogicApps.JavaScript.Jint.Handler

let mockSim () = Foq.Mock<SimulatorContext>().Create()

let makeResult () = ref (Error "No result")

let defaultAction =
    { code = Some OK
      status = Succeeded
      error = None
      inputs = None
      outputs = None
      name = "Action"
      endTime = ""
      startTime = ""
      trackedProperties = None
      trackingId = ""
      clientTrackingId = "" }

let makeActions seq =
    seq
    |> Seq.map (fun (k, v) ->
        k,
        { defaultAction with
            name = k
            outputs = Some v })
    |> OrderedMap.ofSeq

let makeRequest actions code result =
    ScriptExecution(
        { actions = actions
          language = JavaScript
          source = Inline code
          trigger = CompletedTrigger.create defaultAction
          workflow = WorkflowDetails.Create "123" "TestWorkflow" "0abc" },
        result
    )

let getError result =
    match result with
    | Error e -> e
    | _ -> failwith "Expected error"

[<Test>]
let ``Test that integer and float can be added together`` () =
    let actions = makeActions [ "action1", Float 1.5; "action2", Integer 1 ]

    let code =
        """
        let {action1, action2} = workflowContext.actions;
        return action1.outputs + action2.outputs;
        """

    let result = makeResult ()

    let request = makeRequest actions code result

    test <@ jintJavascriptHandler (mockSim ()) request = true @>
    test <@ result.Value = Ok(Float 2.5) @>

[<TestCase("""throw new Error("Ouch!");""", "Ouch!")>]
[<TestCase("""x.x.x.x.x""", "x is not defined")>]
[<TestCase(""")""", "Unexpected token")>]
// TODO: you can easily overflow the stack and bring down the whole test host.
// We should run the JavaScript interpreter in a thread with its own stack.
let ``Test that script execution exceptions do not propagate`` code (errorText: string) =
    let result = makeResult ()
    let request = makeRequest OrderedMap.empty code result

    test <@ jintJavascriptHandler (mockSim ()) request = true @>
    test <@ (getError result.Value).Contains errorText @>
