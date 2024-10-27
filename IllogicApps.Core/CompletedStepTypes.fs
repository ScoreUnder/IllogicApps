module IllogicApps.Core.CompletedStepTypes

open System
open System.Globalization
open System.Text.Json.Nodes
open System.Text.Json.Serialization

type Status =
    | Succeeded
    | Failed
    | Skipped
    | TimedOut
    | Cancelled

let makeNewWorkflowRunId() =
    // This isn't a GUID but oh well
    Guid.NewGuid().ToString()

let makeNewTrackingId() =
    Guid.NewGuid().ToString()

let stringOfDateTime (dt: DateTime) =
    dt.ToString("o", CultureInfo.InvariantCulture)

type CompletedAction(name: string, status: Status, startTime: string, ?workflowRunId: string) =
    let workflowRunId = defaultArg workflowRunId (makeNewWorkflowRunId())

    [<JsonPropertyName("name")>]
    member val Name = name with get, set

    [<JsonPropertyName("inputs")>]
    member val Inputs: JsonNode option = None with get, set

    [<JsonPropertyName("outputs")>]
    member val Outputs: JsonNode option = None with get, set

    [<JsonPropertyName("startTime")>]
    member val StartTime = startTime with get, set

    [<JsonPropertyName("endTime")>]
    member val EndTime = stringOfDateTime DateTime.UtcNow with get, set

    [<JsonPropertyName("trackingId")>]
    member val TrackingId = makeNewTrackingId() with get, set

    [<JsonPropertyName("clientTrackingId")>]
    member val ClientTrackingId = workflowRunId with get, set

    [<JsonPropertyName("status")>]
    member val Status = status with get, set

type CompletedTrigger(name: string, status: Status, startTime: string, ?workflowRunId: string) as this =
    inherit CompletedAction(name, status, startTime, ?workflowRunId = workflowRunId)

    [<JsonPropertyName("scheduledTime")>]
    member val ScheduledTime: string option = None with get, set

    [<JsonPropertyName("originHistoryName")>]
    member val OriginHistoryName = this.ClientTrackingId with get, set

    [<JsonPropertyName("code")>]
    member val Code: string option = None with get, set
