namespace LogicAppUnit

open System
open System.Collections.Generic
open System.Net
open System.Net.Http

open System.Text
open IllogicApps.Compat.LogicAppUnit.InternalHelper
open IllogicApps.Compat.LogicAppUnit.LogicAppUnitConversions
open IllogicApps.Compat.LogicAppUnit.NewtonsoftJsonConversions
open IllogicApps.Core
open IllogicApps.Core.CompletedStepTypes
open IllogicApps.Core.ExternalServiceTypes
open IllogicApps.Core.ExternalServiceTypeConversions
open IllogicApps.Json
open IllogicApps.Simulator
open IllogicApps.Simulator.Parameters
open LogicAppUnit.Mocking

(*
The interface specified in this comment is work that was done by the original LogicAppUnit author,
and is licensed under the MIT license; see Translations/LICENSE_LogicAppUnit.md

Public-facing interface in LogicAppUnit:

public class implementing ITestRunner and IDisposable
no public-facing constructors
*)
type TestRunner
    internal
    (
        mockDefinition: MockDefinition,
        workflows: (string * LogicAppSpec.Root) seq,
        workflowName: string,
        appSettings: OrderedMap<string, string>,
        parameters: OrderedMap<string, Parameter>
    ) as this =
    let workflows = workflows |> Array.ofSeq
    let mutable apiMocks: Func<HttpRequestMessage, HttpResponseMessage> = null
    let mutable mockRequests: MockRequest list = []
    let mutable mockResponses: MockResponse list = []
    let mutable simulators: Simulator list = []

    let mySim () = simulators |> List.head

    let addMockResponse mockResponse =
        mockResponses <- mockResponse :: mockResponses
        mockResponse


    let defaultHandler (overallResponse: HttpResponseMessage option ref) originWorkflowName sim =
        function
        | HttpResponse(response) when overallResponse.Value.IsNone && workflowName = originWorkflowName ->
            overallResponse.Value <- Some <| netHttpResponseMessageOfHttpRequestReply response
            true
        | HttpRequest(request, reply) ->
            let request = netHttpRequestMessageOfHttpRequest request

            let result =
                mockDefinition
                    .MatchRequestAndBuildResponseAsync(request)
                    .GetAwaiter()
                    .GetResult()

            reply.Value <- result |> httpRequestReplyOfNetHttpResponseMessage
            true
        | Workflow(request, reply) ->
            let uri = $"{this.MockHostUri}/{request.workflowId.TrimStart('/')}"
            let requestStr = request |> jsonOfWorkflowRequest |> Conversions.stringOfJson
            let content = new StringContent(requestStr, Encoding.UTF8, "application/json")
            let netHttpRequest = new HttpRequestMessage(HttpMethod.Post, uri, Content = content)

            let result =
                mockDefinition
                    .MatchRequestAndBuildResponseAsync(netHttpRequest)
                    .GetAwaiter()
                    .GetResult()

            reply.Value <- result |> httpRequestReplyOfNetHttpResponseMessage
            true
        | ServiceProvider(serviceProviderReq, reply) ->
            let config = serviceProviderReq.serviceProviderConfiguration
            let uri = $"{this.MockHostUri}/{config.serviceProviderId.TrimStart('/')}"
            let parameters = serviceProviderReq.parameters
            let contentType = parameters |> JsonTree.getType |> contentTypeOfJsonType
            let requestStr = parameters |> Conversions.stringOfJson

            let content =
                new StringContent(requestStr, Encoding.UTF8, contentType |> Option.defaultValue "text/plain")

            let netHttpRequest = new HttpRequestMessage(HttpMethod.Post, uri, Content = content)

            let result =
                mockDefinition
                    .MatchRequestAndBuildResponseAsync(netHttpRequest)
                    .GetAwaiter()
                    .GetResult()

            reply.Value <- result |> httpRequestReplyOfNetHttpResponseMessage
            true
        | _ -> false

    let printOutputSummary (sims: Simulator list) =
        Console.WriteLine("Variables and outputs:")

        sims
        |> List.iter (fun sim ->
            Console.WriteLine($"Run: {sim.WorkflowDetails.run.id}")
            Console.WriteLine($"Run status: {sim.TerminationStatus}")

            sim.Variables
            |> Seq.iter (fun (KeyValue(k, v)) ->
                let pretty = Conversions.prettyStringOfJson v
                Console.WriteLine($"Variable {k} = {pretty}"))

            sim.ActionResults
            |> Seq.iter (fun (KeyValue(k, v)) ->
                let pretty = v |> jsonOfCompletedAction |> Conversions.prettyStringOfJson
                Console.WriteLine($"Action {k} = {pretty}"))

            Console.WriteLine())

        Console.WriteLine("End variables and outputs.")

    member _.MockHostUri = "http://mockHost.localhost"

    member this.TriggerWorkflow
        (queryParams: Dictionary<string, string>, content: HttpContent, requestHeaders: Dictionary<string, string>)
        : HttpResponseMessage =
        // First, let's wash our hands
        let queryParams = queryParams |> Option.ofObj
        let content = content |> sanitiseNull (new ByteArrayContent(Array.empty<byte>))
        let requestHeaders = requestHeaders |> Option.ofObj

        // Grab details of HTTP content
        let contentType =
            content.Headers.ContentType
            |> Option.ofObj
            |> Option.map _.MediaType
            |> Option.defaultWith (fun () ->
                requestHeaders
                |> Option.bind (fun h ->
                    match h.TryGetValue("Content-Type") with
                    | true, x -> Some x
                    | _ -> None)
                |> Option.defaultValue "text/plain")

        let contentString = content.ReadAsStringAsync().GetAwaiter().GetResult()

        // Create the trigger outputs
        let request =
            { HttpTrigger.queries = queryParams |> Option.map OrderedMap.CreateRange
              headers = requestHeaders |> Option.map OrderedMap.CreateRange
              body =
                if contentString.Length = 0 then
                    None
                else
                    decodeBodyByContentType contentType contentString |> Some }

        let overallResponse = ref None
        mockDefinition.TestRunStarting()

        // TODO: Handle MaxWorkflowExecutionDuration?

        let sims =
            request
            |> jsonOfHttpTrigger
            |> Some
            |> WorkflowFamily.buildWorkflowFamily
                (fun options handler ->
                    { options with
                        parameters = parameters
                        appConfig = appSettings
                        isBugForBugAccurate = true
                        externalServiceHandlers =
                            [ handler
                              defaultHandler overallResponse options.workflowName
                              IllogicApps.JavaScript.Jint.jintJavascriptHandler
                              ExternalServices.loggingHandler ] })
                workflows
                workflowName

        simulators <- sims

        mockDefinition.TestRunComplete()
        printOutputSummary sims

        match overallResponse.Value with
        | Some r -> r
        | None ->
            let status =
                match sims |> List.head |> _.TerminationStatus with
                | Some Succeeded -> HttpStatusCode.Accepted
                | _ -> HttpStatusCode.BadGateway

            new HttpResponseMessage(status)

    // Let's be nice to those who inherit from us
    abstract Dispose: disposing: bool -> unit
    override this.Dispose(disposing: bool) = ()

    override this.Finalize() = this.Dispose(false)

    interface IDisposable with
        member this.Dispose() = this.Dispose(true)

    interface ITestRunner with
        member this.AddApiMocks
            with set value = apiMocks <- value

        member this.AddMockResponse(mockRequestMatcher) =
            addMockResponse (MockResponse(null, mockRequestMatcher))

        member this.AddMockResponse(name, mockRequestMatcher) =
            addMockResponse (MockResponse(name, mockRequestMatcher))

        member this.ExceptionWrapper(assertion) = assertion.Invoke()

        member this.GetWorkflowAction(actionName) =
            mySim().ActionResults.[actionName]
            |> jsonOfCompletedAction
            |> newtonsoftJsonOfIllogicJson

        member this.GetWorkflowActionInput(actionName) =
            mySim().ActionResults.[actionName].inputs
            |> Conversions.jsonOfOption
            |> newtonsoftJsonOfIllogicJson

        member this.GetWorkflowActionInput(actionName, repetitionNumber) = failwith "todo"

        member this.GetWorkflowActionOutput(actionName) =
            mySim().ActionResults.[actionName].outputs
            |> Conversions.jsonOfOption
            |> newtonsoftJsonOfIllogicJson

        member this.GetWorkflowActionOutput(actionName, repetitionNumber) = failwith "todo"
        member this.GetWorkflowActionRepetition(actionName, repetitionNumber) = failwith "todo"
        member this.GetWorkflowActionRepetitionCount(actionName) = failwith "todo"

        member this.GetWorkflowActionStatus(actionName) =
            mySim().ActionResults.[actionName].status |> actionStatusOfIllogic

        member this.GetWorkflowActionStatus(actionName, repetitionNumber) = failwith "todo"
        member this.GetWorkflowActionTrackedProperties(actionName) = failwith "todo"
        member this.GetWorkflowActionTrackedProperties(actionName, repetitionNumber) = failwith "todo"

        member this.MockRequests = mockRequests |> List.rev |> List<MockRequest>

        member this.TriggerWorkflow(method, requestHeaders) =
            this.TriggerWorkflow(null, null, requestHeaders)

        member this.TriggerWorkflow
            (queryParams: Dictionary<string, string>, method: HttpMethod, requestHeaders: Dictionary<string, string>)
            : HttpResponseMessage =
            this.TriggerWorkflow(queryParams, null, requestHeaders)

        member this.TriggerWorkflow
            (
                queryParams: Dictionary<string, string>,
                method: HttpMethod,
                relativePath: string,
                requestHeaders: Dictionary<string, string>
            ) : HttpResponseMessage =
            this.TriggerWorkflow(queryParams, null, requestHeaders)

        member this.TriggerWorkflow
            (content: HttpContent, method: HttpMethod, requestHeaders: Dictionary<string, string>)
            : HttpResponseMessage =
            this.TriggerWorkflow(null, content, requestHeaders)

        member this.TriggerWorkflow
            (content: HttpContent, method: HttpMethod, relativePath: string, requestHeaders: Dictionary<string, string>) : HttpResponseMessage =
            this.TriggerWorkflow(null, content, requestHeaders)

        member this.TriggerWorkflow(queryParams, content, method, relativePath, requestHeaders) =
            this.TriggerWorkflow(queryParams, content, requestHeaders)

        member this.WaitForAsynchronousResponse(maxTimeoutSeconds: int) : unit = failwith "todo"
        member this.WaitForAsynchronousResponse(maxTimeout: TimeSpan) : unit = failwith "todo"
        member this.WorkflowClientTrackingId = mySim().TriggerResult.originHistoryName
        member this.WorkflowRunId = mySim().WorkflowDetails.run.name

        member this.WorkflowRunStatus =
            mySim().TerminationStatus
            |> Option.defaultValue Skipped
            |> workflowRunStatusOfIllogic

        member this.WorkflowTerminationCode = failwith "todo"
        member this.WorkflowTerminationMessage = failwith "todo"
        member this.WorkflowWasTerminated = failwith "todo"
