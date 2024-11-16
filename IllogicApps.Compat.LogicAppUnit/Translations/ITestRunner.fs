// NOTICE: This file is a translation of an equivalent file in the LogicAppUnit project, and as such is licensed
// under the same terms as that project. The license can be found in this project's directory.
namespace LogicAppUnit

open System
open System.Collections.Generic
open System.Net.Http
open System.Runtime.InteropServices
open LogicAppUnit.Mocking
open Newtonsoft.Json.Linq

type ITestRunner =
    inherit IDisposable

    abstract AddApiMocks: Func<HttpRequestMessage, HttpResponseMessage> with set
    abstract AddMockResponse: mockRequestMatcher: IMockRequestMatcher -> IMockResponse
    abstract AddMockResponse: name: string * mockRequestMatcher: IMockRequestMatcher -> IMockResponse
    abstract MockRequests: List<MockRequest> with get
    abstract WorkflowRunId: string with get
    abstract WorkflowClientTrackingId: string with get
    abstract WorkflowRunStatus: WorkflowRunStatus with get
    abstract WorkflowWasTerminated: bool with get
    abstract WorkflowTerminationCode: int Nullable with get
    abstract WorkflowTerminationMessage: string with get
    abstract GetWorkflowAction: actionName: string -> JToken
    abstract GetWorkflowActionStatus: actionName: string -> ActionStatus
    abstract GetWorkflowActionInput: actionName: string -> JToken
    abstract GetWorkflowActionOutput: actionName: string -> JToken
    abstract GetWorkflowActionRepetitionCount: actionName: string -> int
    abstract GetWorkflowActionTrackedProperties: actionName: string -> Dictionary<string, string>
    abstract GetWorkflowActionRepetition: actionName: string * repetitionNumber: int -> JToken
    abstract GetWorkflowActionStatus: actionName: string * repetitionNumber: int -> ActionStatus
    abstract GetWorkflowActionInput: actionName: string * repetitionNumber: int -> JToken
    abstract GetWorkflowActionOutput: actionName: string * repetitionNumber: int -> JToken

    abstract GetWorkflowActionTrackedProperties:
        actionName: string * repetitionNumber: int -> Dictionary<string, string>

    abstract TriggerWorkflow:
        method: HttpMethod *
        [<Optional; DefaultParameterValue(null: Dictionary<string, string>)>] requestHeaders: Dictionary<string, string> ->
            HttpResponseMessage

    abstract TriggerWorkflow:
        queryParams: Dictionary<string, string> *
        method: HttpMethod *
        [<Optional; DefaultParameterValue(null: Dictionary<string, string>)>] requestHeaders: Dictionary<string, string> ->
            HttpResponseMessage

    abstract TriggerWorkflow:
        queryParams: Dictionary<string, string> *
        method: HttpMethod *
        relativePath: string *
        [<Optional; DefaultParameterValue(null: Dictionary<string, string>)>] requestHeaders: Dictionary<string, string> ->
            HttpResponseMessage

    abstract TriggerWorkflow:
        content: HttpContent *
        method: HttpMethod *
        [<Optional; DefaultParameterValue(null: Dictionary<string, string>)>] requestHeaders: Dictionary<string, string> ->
            HttpResponseMessage

    abstract TriggerWorkflow:
        content: HttpContent *
        method: HttpMethod *
        relativePath: string *
        [<Optional; DefaultParameterValue(null: Dictionary<string, string>)>] requestHeaders: Dictionary<string, string> ->
            HttpResponseMessage

    abstract TriggerWorkflow:
        queryParams: Dictionary<string, string> *
        content: HttpContent *
        method: HttpMethod *
        relativePath: string *
        [<Optional; DefaultParameterValue(null: Dictionary<string, string>)>] requestHeaders: Dictionary<string, string> ->
            HttpResponseMessage

    abstract WaitForAsynchronousResponse: maxTimeoutSeconds: int -> unit
    abstract WaitForAsynchronousResponse: maxTimeout: TimeSpan -> unit
    abstract ExceptionWrapper: assertion: Action -> unit
