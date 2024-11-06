module IllogicApps.Core.CompletedStepTypes

open System
open System.Globalization
open IllogicApps.Json

type Status =
    | Succeeded
    | Failed
    | Skipped
    | TimedOut
    | Cancelled

let stringOfStatus =
    function
    | Succeeded -> "Succeeded"
    | Failed -> "Failed"
    | Skipped -> "Skipped"
    | TimedOut -> "TimedOut"
    | Cancelled -> "Cancelled"

let makeNewWorkflowRunId () =
    // This isn't a GUID but oh well
    Guid.NewGuid().ToString()

let makeNewTrackingId () = Guid.NewGuid().ToString()

let stringOfDateTime (dt: DateTime) =
    dt.ToString("o", CultureInfo.InvariantCulture)

type CompletedAction =
    { name: string
      inputs: JsonTree
      outputs: JsonTree
      startTime: string
      endTime: string
      trackingId: string
      clientTrackingId: string
      status: Status }

module CompletedAction =
    let inline create name startTime =
        { name = name
          inputs = Null
          outputs = Null
          startTime = startTime
          endTime = stringOfDateTime DateTime.UtcNow
          trackingId = makeNewTrackingId ()
          clientTrackingId = makeNewWorkflowRunId ()
          status = Succeeded }

let orderedMapBuilderOfCompletedAction (ca: CompletedAction) =
    OrderedMap
        .Builder()
        .Add("name", String ca.name)
        .MaybeAdd("inputs", ca.inputs)
        .MaybeAdd("outputs", ca.outputs)
        .Add("startTime", String ca.startTime)
        .Add("endTime", String ca.endTime)
        .Add("trackingId", String ca.trackingId)
        .Add("clientTrackingId", String ca.clientTrackingId)
        .Add("status", String(ca.status |> stringOfStatus))

let jsonOfCompletedAction (ca: CompletedAction) =
    Object((orderedMapBuilderOfCompletedAction ca).Build())

type CompletedTrigger =
    { action: CompletedAction
      scheduledTime: string option
      originHistoryName: string
      code: string option }

let orderedMapBuilderOfCompletedTrigger (ct: CompletedTrigger) =
    (orderedMapBuilderOfCompletedAction ct.action)
        .MaybeAdd("scheduledTime", ct.scheduledTime)
        .Add("originHistoryName", String ct.originHistoryName)
        .MaybeAdd("code", ct.code)

let jsonOfCompletedTrigger (ct: CompletedTrigger) =
    Object((orderedMapBuilderOfCompletedTrigger ct).Build())

module CompletedTrigger =
    let inline create completedAction =
        { action = completedAction
          scheduledTime = None
          originHistoryName = completedAction.clientTrackingId
          code = None }
