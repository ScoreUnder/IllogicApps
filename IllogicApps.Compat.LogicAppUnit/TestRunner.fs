namespace LogicAppUnit

open System
open System.Collections.Generic
open System.Net
open System.Net.Http

open IllogicApps.Compat.LogicAppUnit.InternalHelper
open IllogicApps.Compat.LogicAppUnit.LogicAppUnitConversions
open IllogicApps.Compat.LogicAppUnit.NewtonsoftJsonConversions
open IllogicApps.Core
open IllogicApps.Core.CompletedStepTypes
open IllogicApps.Core.ExternalServiceTypes
open IllogicApps.Core.ExternalServiceTypeConversions
open IllogicApps.Core.HttpModel.HttpParsing
open IllogicApps.Core.HttpModel.HttpWriting
open IllogicApps.Core.HttpModel.RetryPolicy
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
    ) =
    [<Literal>]
    static let MOCK_HOST_URI = "http://mockHost.localhost"

    let workflows = workflows |> Array.ofSeq // Eagerly evaluate the workflows so that exceptions are thrown early
    let mutable simulators: Simulator list = []
    // TODO: Implement asyncResponseTimeout
    let mutable asyncResponseTimeout = None

    let mySim () = simulators |> List.head

    let compatibleJsonOfWorkflowRequest (req: WorkflowRequest) =
        OrderedMap
            .Builder()
            .Add(
                "host",
                Conversions.createObject [ "workflow", Conversions.createObject [ "id", String req.workflowId ] ]
            )
            .Add("headers", req.headers |> Object)
            .MaybeAdd("body", req.body |> Conversions.optionOfJson)
            .MaybeAdd("retryPolicy", req.retryPolicy |> Option.map jsonOfRetryPolicy)
            .Build()
        |> Object

    let defaultHandler (overallResponse: HttpResponseMessage option ref) originWorkflowName (sim: SimulatorContext) =
        let getMockResponse request =
            try
                mockDefinition
                    .MatchRequestAndBuildResponseAsync(request)
                    .GetAwaiter()
                    .GetResult()
            with e ->
                Console.WriteLine(string e)
                new HttpResponseMessage(HttpStatusCode.InternalServerError)

        let mockAsFakeHttpRequest actionName json =
            let uri = $"{MOCK_HOST_URI}/{actionName}"
            let content = netHttpContentOfJson json
            let netHttpRequest = new HttpRequestMessage(HttpMethod.Post, uri, Content = content)

            LogicAppActionSupport.makeWorkflowHttpRequestHeaders actionName sim
            |> List.iter (fun (k, v) -> netHttpRequest.Headers.TryAddWithoutValidation(k, v) |> ignore)

            netHttpRequest

        function
        | HttpResponse(response) when overallResponse.Value.IsNone && workflowName = originWorkflowName ->
            // TODO: `workflowName = originWorkflowName` is not recursion-friendly
            overallResponse.Value <- Some <| netHttpResponseMessageOfHttpRequestReply response
            true
        | HttpRequest(request, reply) ->
            let request = netHttpRequestMessageOfHttpServiceRequest request
            let result = getMockResponse request

            reply.Value <- result |> httpRequestReplyOfNetHttpResponseMessage
            true
        | Workflow(request, reply) ->
            let fakeHttpRequest =
                mockAsFakeHttpRequest request.actionName (compatibleJsonOfWorkflowRequest request)

            let result = getMockResponse fakeHttpRequest
            reply.Value <- result |> httpRequestReplyOfNetHttpResponseMessage
            true
        | ServiceProvider(serviceProviderReq, reply) ->
            let fakeHttpRequest =
                mockAsFakeHttpRequest serviceProviderReq.actionName serviceProviderReq.parameters

            let result = getMockResponse fakeHttpRequest
            reply.Value <- result |> httpRequestReplyOfNetHttpResponseMessage
            true
        | InvokeFunction(functionReq, reply) ->
            let fakeHttpRequest =
                mockAsFakeHttpRequest functionReq.actionName functionReq.parameters

            let result = getMockResponse fakeHttpRequest
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

            match sim.TerminationStatus with
            | Ok Skipped -> Console.WriteLine("Workflow was not run. (???)")
            | Ok status -> Console.WriteLine($"Workflow completed with status: {stringOfStatus status}")
            | Error(status, e) ->
                Console.WriteLine(
                    $"Workflow terminated with status: {stringOfStatus status} and termination error: {e}"
                )

            Console.WriteLine())

        Console.WriteLine("End variables and outputs.")

    member _.MockHostUri = MOCK_HOST_URI

    member this.TriggerWorkflow
        (
            queryParams: Dictionary<string, string>,
            content: HttpContent,
            method: HttpMethod,
            relativePath: string,
            requestHeaders: Dictionary<string, string>
        ) : HttpResponseMessage =

        // First, let's wash our hands
        let queryParams = queryParams |> Option.ofObj
        let content = content |> sanitiseNull (new ByteArrayContent(Array.empty<byte>))
        let method = method |> sanitiseNull HttpMethod.Post
        let relativePath = relativePath |> sanitiseNull ""
        let requestHeaders = requestHeaders |> Option.ofObj

        // Grab details of HTTP content
        let contentType =
            content.Headers.ContentType
            |> Option.ofObj
            |> Option.map string
            |> Option.orElseWith (fun () ->
                requestHeaders
                |> Option.bind (fun h ->
                    match h.TryGetValue("Content-Type") with
                    | true, x -> Some x
                    | _ -> None))

        let contentBytes = content.ReadAsByteArrayAsync().GetAwaiter().GetResult()

        // Create the trigger outputs
        let request =
            { HttpRequest.method = method
              relativePath = relativePath
              queries = queryParams |> Option.map OrderedMap.CreateRange
              headers = requestHeaders |> Option.map OrderedMap.CreateRange
              body = decodeOptionalBodyByContentType contentType contentBytes }

        // Figure out if there is a Response action in the main workflow
        let hasResponseAction =
            workflows
            |> Array.pick (fun (name, root) ->
                if name = workflowName then
                    Some root.definition.actions
                else
                    None)
            |> LogicAppValidation.containsResponseAction

        // Start test
        let overallResponse = ref None
        mockDefinition.TestRunStarting()

        // TODO: Handle MaxWorkflowExecutionDuration?

        let sims =
            request
            |> TriggerCompletion.Invoked
            |> WorkflowFamily.buildWorkflowFamily
                (fun options handler ->
                    { options with
                        parameters = parameters
                        appConfig = appSettings
                        isBugForBugAccurate = true
                        externalServiceHandlers =
                            [ handler
                              defaultHandler overallResponse options.workflowName
                              IllogicApps.JavaScript.Jint.Handler.jintJavascriptHandler
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
                | _ when not hasResponseAction -> HttpStatusCode.Accepted
                | Ok Succeeded
                | Error(Succeeded, _) -> HttpStatusCode.Accepted
                | _ -> HttpStatusCode.BadGateway

            new HttpResponseMessage(status)

    // Let's be nice to those who inherit from us
    abstract Dispose: disposing: bool -> unit
    override this.Dispose(_: bool) = ()

    override this.Finalize() = this.Dispose(false)

    interface IDisposable with
        member this.Dispose() = this.Dispose(true)

    interface ITestRunner with
        member this.AddApiMocks
            with set value = mockDefinition.MockResponseDelegate <- value

        member this.AddMockResponse(mockRequestMatcher) =
            mockDefinition.AddMockResponse(mockRequestMatcher)

        member this.AddMockResponse(name, mockRequestMatcher) =
            mockDefinition.AddMockResponse(name, mockRequestMatcher)

        member this.ExceptionWrapper(assertion) = assertion.Invoke()

        member this.GetWorkflowAction(actionName) =
            mySim().ActionResults.[actionName]
            |> jsonOfCompletedAction
            |> newtonsoftJsonOfIllogicJson

        member this.GetWorkflowActionInput(actionName) =
            mySim().ActionResults.[actionName].inputs
            |> Option.get
            |> newtonsoftJsonOfIllogicJson

        member this.GetWorkflowActionInput(actionName, repetitionNumber) =
            mySim().GetActionRepetitions actionName
            |> List.item (repetitionNumber - 1)
            |> snd
            |> _.inputs
            |> Option.get
            |> newtonsoftJsonOfIllogicJson

        member this.GetWorkflowActionOutput(actionName) =
            mySim().ActionResults.[actionName].outputs
            |> Option.get
            |> newtonsoftJsonOfIllogicJson

        member this.GetWorkflowActionOutput(actionName, repetitionNumber) =
            mySim().GetActionRepetitions actionName
            |> List.item (repetitionNumber - 1)
            |> snd
            |> _.outputs
            |> Option.get
            |> newtonsoftJsonOfIllogicJson

        member this.GetWorkflowActionRepetition(actionName, repetitionNumber) =
            mySim().GetActionRepetitions actionName
            |> List.item (repetitionNumber - 1)
            |> snd
            |> jsonOfCompletedAction
            |> newtonsoftJsonOfIllogicJson

        member this.GetWorkflowActionRepetitionCount(actionName) =
            mySim().GetActionRepetitions actionName |> List.length

        member this.GetWorkflowActionStatus(actionName) =
            mySim().ActionResults.[actionName].status |> actionStatusOfIllogic

        member this.GetWorkflowActionStatus(actionName, repetitionNumber) =
            mySim().GetActionRepetitions actionName
            |> List.item (repetitionNumber - 1)
            |> snd
            |> _.status
            |> actionStatusOfIllogic

        member this.GetWorkflowActionTrackedProperties(actionName) =
            mySim().ActionResults.[actionName].trackedProperties
            |> Option.map (fun map ->
                map
                |> OrderedMap.toSeq
                |> Seq.map (fun (k, v) -> KeyValuePair(k, Conversions.rawStringOfJson v))
                |> Dictionary<string, string>)
            |> Option.toObj

        member this.GetWorkflowActionTrackedProperties(actionName, repetitionNumber) =
            mySim().GetActionRepetitions actionName
            |> List.item (repetitionNumber - 1)
            |> snd
            |> _.trackedProperties
            |> Option.map (fun map ->
                map
                |> OrderedMap.toSeq
                |> Seq.map (fun (k, v) -> KeyValuePair(k, Conversions.rawStringOfJson v))
                |> Dictionary<string, string>)
            |> Option.toObj

        member this.MockRequests = mockDefinition.MockRequests

        member this.TriggerWorkflow(method, requestHeaders) =
            this.TriggerWorkflow(null, null, method, null, requestHeaders)

        member this.TriggerWorkflow
            (queryParams: Dictionary<string, string>, method: HttpMethod, requestHeaders: Dictionary<string, string>)
            : HttpResponseMessage =
            this.TriggerWorkflow(queryParams, null, method, null, requestHeaders)

        member this.TriggerWorkflow
            (
                queryParams: Dictionary<string, string>,
                method: HttpMethod,
                relativePath: string,
                requestHeaders: Dictionary<string, string>
            ) : HttpResponseMessage =
            this.TriggerWorkflow(queryParams, null, method, relativePath, requestHeaders)

        member this.TriggerWorkflow
            (content: HttpContent, method: HttpMethod, requestHeaders: Dictionary<string, string>)
            : HttpResponseMessage =
            this.TriggerWorkflow(null, content, method, null, requestHeaders)

        member this.TriggerWorkflow
            (content: HttpContent, method: HttpMethod, relativePath: string, requestHeaders: Dictionary<string, string>) : HttpResponseMessage =
            this.TriggerWorkflow(null, content, method, relativePath, requestHeaders)

        member this.TriggerWorkflow(queryParams, content, method, relativePath, requestHeaders) =
            this.TriggerWorkflow(queryParams, content, method, relativePath, requestHeaders)

        member this.WaitForAsynchronousResponse(maxTimeoutSeconds: int) : unit =
            asyncResponseTimeout <- Some(TimeSpan.FromSeconds(float maxTimeoutSeconds))

        member this.WaitForAsynchronousResponse(maxTimeout: TimeSpan) : unit = asyncResponseTimeout <- Some maxTimeout
        member this.WorkflowClientTrackingId = mySim().TriggerResult.clientTrackingId
        member this.WorkflowRunId = mySim().WorkflowDetails.run.name

        member this.WorkflowRunStatus =
            mySim().TerminationStatus
            |> ResultEx.recover id fst
            |> workflowRunStatusOfIllogic

        member this.WorkflowTerminationCode =
            mySim().TerminationStatus
            |> ResultEx.recover (fun _ -> None) Some
            |> Option.bind snd
            |> Option.bind _.code
            |> Option.bind (fun s ->
                match Int32.TryParse s with
                | true, i -> Some i
                | _ -> None)
            |> Option.toNullable

        member this.WorkflowTerminationMessage =
            mySim().TerminationStatus
            |> ResultEx.recover (fun _ -> null) (fun (_, e) -> e |> Option.bind _.message |> Option.toObj)

        member this.WorkflowWasTerminated = mySim().TerminationStatus |> Result.isError
