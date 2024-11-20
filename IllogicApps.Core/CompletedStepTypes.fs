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

    override this.ToString() =
        match this with
        | Succeeded -> "Succeeded"
        | Failed -> "Failed"
        | Skipped -> "Skipped"
        | TimedOut -> "TimedOut"
        | Cancelled -> "Cancelled"


let inline stringOfStatus status = status.ToString()

let statusOfString (s: string) =
    match s.ToLowerInvariant() with
    | "succeeded" -> Succeeded
    | "failed" -> Failed
    | "skipped" -> Skipped
    | "timedout" -> TimedOut
    | "cancelled" -> Cancelled
    | s -> failwithf "Invalid status '%s'" s

type ActionCode =
    // Some of these only appear in "code", some of these only appear in "error"."code", and many appear in both
    // I don't care enough to separate them, if they even are separate
    | NotSpecified // (code) Success for some actions (Scope, Condition, Initialize Variable, etc.)
    | OK // (code) Success for some actions (Compose, Parse JSON, Execute JavaScript Code, etc.)
    | ActionSkipped // (code) Skipped by simulator
    | ActionFailed // (code, error.code) Failure for some actions (Scope...?)
    | ActionBranchingConditionNotSatisfied // (error.code) Skipped because inside a branch that wasn't taken
    | ActionConditionFailed // (error.code) Skipped because runAfter condition was not met
    | ActionDependencyFailed // (error.code) Skipped because runAfter condition was not met (what is the difference???)
    | ActionResponseSkipped // (error.code) Skipped because the workflow is not called by something that waits for a response
    | InlineCodeScriptRuntimeFailure // (code, error.code) Failure to run inline code script
    | InvalidTemplate // (error.code) Failure to run template expression
    | NestedWorkflowDoesNotContainResponseAction // (code, error.code) Failure to run nested workflow due to missing response action
    | NoResponse // (error.code) Failure to receive response from inner workflow
    | Accepted // (code) Ran inner workflow
    | BadGateway // (code) Failure to receive response from inner workflow
    | BadRequest // (code) Failure to run template expression?
    | Terminated // (code) Was sequenced after workflow termination

let stringOfActionCode =
    function
    | NotSpecified -> "NotSpecified"
    | OK -> "OK"
    | ActionSkipped -> "ActionSkipped"
    | ActionFailed -> "ActionFailed"
    | ActionBranchingConditionNotSatisfied -> "ActionBranchingConditionNotSatisfied"
    | ActionConditionFailed -> "ActionConditionFailed"
    | ActionDependencyFailed -> "ActionDependencyFailed"
    | ActionResponseSkipped -> "ActionResponseSkipped"
    | InlineCodeScriptRuntimeFailure -> "InlineCodeScriptRuntimeFailure"
    | InvalidTemplate -> "InvalidTemplate"
    | NestedWorkflowDoesNotContainResponseAction -> "NestedWorkflowDoesNotContainResponseAction"
    | NoResponse -> "NoResponse"
    | Accepted -> "Accepted"
    | BadGateway -> "BadGateway"
    | BadRequest -> "BadRequest"
    | Terminated -> "Terminated"

let statusOfJson (json: JsonTree) =
    json |> Conversions.ensureString |> statusOfString

let makeNewWorkflowRunId () =
    // This isn't a GUID but oh well
    Guid.NewGuid().ToString()

let makeNewTrackingId () = Guid.NewGuid().ToString()

let stringOfDateTime (dt: DateTime) =
    dt.ToString("o", CultureInfo.InvariantCulture)

type ActionError = { code: ActionCode; message: string }

let jsonOfActionError (error: ActionError) =
    OrderedMap
        .Builder()
        .Add("code", String(error.code |> stringOfActionCode))
        .Add("message", String error.message)
        .Build()
    |> JsonTree.Object

type CompletedAction =
    { name: string
      inputs: JsonTree option
      outputs: JsonTree option
      trackedProperties: OrderedMap<string, JsonTree> option
      startTime: string
      endTime: string
      trackingId: string
      clientTrackingId: string
      status: Status
      code: ActionCode option
      error: ActionError option }

module CompletedAction =
    let inline create name startTime =
        { name = name
          inputs = None
          outputs = None
          trackedProperties = None
          startTime = startTime
          endTime = stringOfDateTime DateTime.UtcNow
          trackingId = makeNewTrackingId ()
          clientTrackingId = makeNewWorkflowRunId ()
          status = Succeeded
          code = None
          error = None }

let orderedMapBuilderOfCompletedAction (ca: CompletedAction) =
    OrderedMap
        .Builder()
        .Add("name", String ca.name)
        .MaybeAdd("inputs", ca.inputs)
        .MaybeAdd("outputs", ca.outputs)
        .MaybeAdd("trackedProperties", Option.map Object ca.trackedProperties)
        .Add("startTime", String ca.startTime)
        .Add("endTime", String ca.endTime)
        .Add("trackingId", String ca.trackingId)
        .Add("clientTrackingId", String ca.clientTrackingId)
        .Add("status", String(ca.status |> stringOfStatus))
        .MaybeAdd("code", ca.code |> Option.map (fun c -> c |> stringOfActionCode |> String))
        .MaybeAdd("error", ca.error |> Option.map jsonOfActionError)

let jsonOfCompletedAction (ca: CompletedAction) =
    Object((orderedMapBuilderOfCompletedAction ca).Build())

type CompletedTrigger =
    { action: CompletedAction
      scheduledTime: string option
      originHistoryName: string }

let orderedMapBuilderOfCompletedTrigger (ct: CompletedTrigger) =
    (orderedMapBuilderOfCompletedAction ct.action)
        .MaybeAdd("scheduledTime", ct.scheduledTime)
        .Add("originHistoryName", String ct.originHistoryName)

let jsonOfCompletedTrigger (ct: CompletedTrigger) =
    Object((orderedMapBuilderOfCompletedTrigger ct).Build())

module CompletedTrigger =
    let inline create completedAction =
        { action = completedAction
          scheduledTime = None
          originHistoryName = completedAction.clientTrackingId }

type TerminateRunError =
    { code: string option
      message: string option }

let jsonOfTerminateRunError (error: TerminateRunError) =
    OrderedMap
        .Builder()
        .MaybeAdd("code", error.code)
        .MaybeAdd("message", error.message)
        .Build()
    |> JsonTree.Object

let terminateRunErrorOfJson json =
    { code = JsonTree.tryGetKey "code" json |> Option.map Conversions.ensureString
      message = JsonTree.tryGetKey "message" json |> Option.map Conversions.ensureString }
