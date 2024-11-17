// NOTICE: This file is a translation of equivalent files in the LogicAppUnit project, and as such is licensed
// under the same terms as that project. The license can be found in this project's directory.
namespace LogicAppUnit.Mocking

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Reflection
open System.Runtime.ExceptionServices
open System.Text
open System.Threading.Tasks

open IllogicApps.Compat.LogicAppUnit
open IllogicApps.Json
open Newtonsoft.Json.Linq

open LogicAppUnit
open LogicAppUnit.Helper

type PathMatchType =
    | Exact = 0
    | Contains = 1
    | EndsWith = 2

[<AllowNullLiteral>]
type IMockRequestMatcher =
    abstract UsingAnyMethod: unit -> IMockRequestMatcher
    abstract UsingGet: unit -> IMockRequestMatcher
    abstract UsingPost: unit -> IMockRequestMatcher
    abstract UsingPut: unit -> IMockRequestMatcher
    abstract UsingPatch: unit -> IMockRequestMatcher
    abstract UsingDelete: unit -> IMockRequestMatcher
    abstract UsingMethod: [<ParamArray>] methods: HttpMethod array -> IMockRequestMatcher
    abstract FromAction: [<ParamArray>] actionNames: string array -> IMockRequestMatcher
    abstract WithPath: matchType: PathMatchType * [<ParamArray>] paths: string array -> IMockRequestMatcher
    abstract WithHeader: name: string -> IMockRequestMatcher
    abstract WithHeader: name: string * value: string -> IMockRequestMatcher
    abstract WithContentType: [<ParamArray>] contentTypes: string array -> IMockRequestMatcher
    abstract WithQueryParam: name: string -> IMockRequestMatcher
    abstract WithQueryParam: name: string * value: string -> IMockRequestMatcher
    abstract WithMatchCount: [<ParamArray>] matchCounts: int array -> IMockRequestMatcher
    abstract WithNotMatchCount: [<ParamArray>] matchCounts: int array -> IMockRequestMatcher
    abstract WithContentAsString: requestContentMatch: Func<string, bool> -> IMockRequestMatcher
    abstract WithContentAsJson: requestContentMatch: Func<JToken, bool> -> IMockRequestMatcher

[<AllowNullLiteral>]
type IMockResponseBuilder =
    abstract WithSuccess: unit -> IMockResponseBuilder
    abstract WithAccepted: unit -> IMockResponseBuilder
    abstract WithNoContent: unit -> IMockResponseBuilder
    abstract WithUnauthorized: unit -> IMockResponseBuilder
    abstract WithNotFound: unit -> IMockResponseBuilder
    abstract WithInternalServerError: unit -> IMockResponseBuilder
    abstract WithStatusCode: statusCode: HttpStatusCode -> IMockResponseBuilder
    abstract WithHeader: name: string * value: string -> IMockResponseBuilder
    abstract AfterDelay: secondsDelay: int -> IMockResponseBuilder
    abstract AfterDelay: delay: TimeSpan -> IMockResponseBuilder
    abstract AfterDelay: secondsMin: int * secondsMax: int -> IMockResponseBuilder
    abstract AfterDelay: min: TimeSpan * max: TimeSpan -> IMockResponseBuilder
    abstract WithContent: content: Func<HttpContent> -> IMockResponseBuilder
    abstract WithContentAsJson: jsonStream: Stream -> IMockResponseBuilder
    abstract WithContentAsJson: jsonString: string -> IMockResponseBuilder
    abstract WithContentAsJson: body: obj -> IMockResponseBuilder
    abstract WithContentAsJson: resourceName: string * containingAssembly: Assembly -> IMockResponseBuilder
    abstract WithContentAsPlainText: value: string -> IMockResponseBuilder
    abstract WithContentAsPlainText: stream: Stream -> IMockResponseBuilder
    abstract WithContentAsPlainText: resourceName: string * containingAssembly: Assembly -> IMockResponseBuilder
    abstract WithContent: value: string * contentType: string * encoding: Encoding -> IMockResponseBuilder
    abstract WithContent: stream: Stream * contentType: string -> IMockResponseBuilder

    abstract WithContent:
        resourceName: string * containingAssembly: Assembly * contentType: string -> IMockResponseBuilder

    abstract ThrowsException: exceptionToThrow: Exception -> IMockResponseBuilder

type IMockResponse =
    abstract RespondWith: mockResponseBuilder: IMockResponseBuilder -> unit
    abstract RespondWithDefault: unit -> unit

// From this point onwards I apologise to the author of LogicAppUnit for the quality of the code translation.

type private MockRequestPath =
    { Path: string
      MatchType: PathMatchType }

type internal MockRequestCache(request: HttpRequestMessage) =
    let _request = request
    let mutable _contentAsString: string = null
    let mutable _contentAsJson: JToken = null

    member this.ContentAsStringAsync() =
        task {
            if String.IsNullOrEmpty _contentAsString then
                let! newContent = _request.Content.ReadAsStringAsync()
                _contentAsString <- newContent

            return _contentAsString
        }

    member this.ContentAsJsonAsync() =
        task {
            if _contentAsJson = null then
                let! contentAsString = this.ContentAsStringAsync()
                // TODO: Originally this was _request.Content.ReadAsAsync<JToken>();
                // but I'm having trouble importing the right package for that
                // so I hope this behaves the same way:
                let newContent = JToken.Parse contentAsString
                _contentAsJson <- newContent

            return _contentAsJson
        }

type internal MockRequestMatchResult(isMatch: bool, matchLog: string) =
    new(isMatch) = MockRequestMatchResult(isMatch, "")

    member _.IsMatch = isMatch
    member _.MatchLog = matchLog

module private MockRequestMatcherModule =
    let inline checkNotEmpty (arr: 'a array) name =
        if arr = null || arr.Length = 0 then
            raise (ArgumentNullException name)

open MockRequestMatcherModule

[<AllowNullLiteral>]
type MockRequestMatcher private () =
    let _requestMethods = List<HttpMethod>()
    let _actionNames = List<string>()
    let _requestPaths = List<MockRequestPath>()
    let _requestHeaders = Dictionary<string, string>()
    let _requestQueryParams = Dictionary<string, string>()
    let _requestContentTypes = List<string>()
    let _requestMatchCounts = List<int>()
    let _requestMatchCountsNot = List<int>()

    let mutable _requestContentStringMatcherDelegate: Func<string, bool> = null
    let mutable _requestContentJsonMatcherDelegate: Func<JToken, bool> = null

    let mutable _requestMatchCounter = 0

    member internal _.MatchCount = _requestMatchCounter

    static member Create() = MockRequestMatcher()

    member this.UsingAnyMethod() =
        _requestMethods.Clear()
        this

    member this.UsingGet() = this.UsingMethod(HttpMethod.Get)
    member this.UsingPost() = this.UsingMethod(HttpMethod.Post)
    member this.UsingPut() = this.UsingMethod(HttpMethod.Put)
    member this.UsingPatch() = this.UsingMethod(HttpMethod.Patch)
    member this.UsingDelete() = this.UsingMethod(HttpMethod.Delete)

    member this.UsingMethod([<ParamArray>] methods: HttpMethod array) =
        checkNotEmpty methods (nameof methods)

        methods
        |> Array.iter (fun method ->
            if not (_requestMethods.Contains method) then
                _requestMethods.Add method)

        this

    member this.FromAction([<ParamArray>] actionNames) =
        checkNotEmpty actionNames (nameof actionNames)

        actionNames
        |> Array.iter (fun actionName ->
            if not (_actionNames.Contains actionName) then
                _actionNames.Add actionName)

        this

    member this.WithPath(matchType, [<ParamArray>] paths) =
        checkNotEmpty paths (nameof paths)

        paths
        |> Array.iter (fun path -> _requestPaths.Add { MatchType = matchType; Path = path })

        this

    member this.WithHeader(name) = this.WithHeader(name, null)

    member this.WithHeader(name, value) =
        ArgumentNullException.ThrowIfNullOrEmpty(name, nameof name)
        _requestHeaders.[name] <- value
        this

    member this.WithContentType([<ParamArray>] contentTypes) =
        checkNotEmpty contentTypes (nameof contentTypes)

        contentTypes
        |> Array.iter (fun contentType ->
            if not (_requestContentTypes.Contains contentType) then
                _requestContentTypes.Add contentType)

        this

    member this.WithQueryParam(name) = this.WithQueryParam(name, null)

    member this.WithQueryParam(name, value) =
        ArgumentNullException.ThrowIfNullOrEmpty(name, nameof name)
        _requestQueryParams.[name] <- value
        this

    member this.WithMatchCount([<ParamArray>] matchCounts) =
        checkNotEmpty matchCounts (nameof matchCounts)

        matchCounts
        |> Array.iter (fun matchCount ->
            if not (_requestMatchCounts.Contains matchCount) then
                _requestMatchCounts.Add matchCount)

        this

    member this.WithNotMatchCount([<ParamArray>] matchCounts) =
        checkNotEmpty matchCounts (nameof matchCounts)

        matchCounts
        |> Array.iter (fun matchCount ->
            if not (_requestMatchCountsNot.Contains matchCount) then
                _requestMatchCountsNot.Add matchCount)

        this

    member this.WithContentAsString(requestContentMatch: Func<string, bool>) =
        ArgumentNullException.ThrowIfNull(requestContentMatch, nameof requestContentMatch)
        _requestContentStringMatcherDelegate <- requestContentMatch
        this

    member this.WithContentAsJson(requestContentMatch: Func<JToken, bool>) =
        ArgumentNullException.ThrowIfNull(requestContentMatch, nameof requestContentMatch)
        _requestContentJsonMatcherDelegate <- requestContentMatch
        this


    interface IMockRequestMatcher with
        member this.UsingAnyMethod() = this.UsingAnyMethod()
        member this.UsingGet() = this.UsingGet()
        member this.UsingPost() = this.UsingPost()
        member this.UsingPut() = this.UsingPut()
        member this.UsingPatch() = this.UsingPatch()
        member this.UsingDelete() = this.UsingDelete()
        member this.UsingMethod(methods) = this.UsingMethod(methods)
        member this.FromAction(actionNames) = this.FromAction(actionNames)
        member this.WithPath(matchType, paths) = this.WithPath(matchType, paths)
        member this.WithHeader(name) = this.WithHeader(name)
        member this.WithHeader(name, value) = this.WithHeader(name, value)
        member this.WithContentType(contentTypes) = this.WithContentType(contentTypes)
        member this.WithQueryParam(name) = this.WithQueryParam(name)
        member this.WithQueryParam(name, value) = this.WithQueryParam(name, value)
        member this.WithMatchCount(matchCounts) = this.WithMatchCount(matchCounts)
        member this.WithNotMatchCount(matchCounts) = this.WithNotMatchCount(matchCounts)

        member this.WithContentAsString(requestContentMatch) =
            this.WithContentAsString(requestContentMatch)

        member this.WithContentAsJson(requestContentMatch) =
            this.WithContentAsJson(requestContentMatch)

    member internal this.MatchRequestAsync
        (request: HttpRequestMessage, requestCache: MockRequestCache)
        : MockRequestMatchResult Task =
        let headerActionName = "x-ms-workflow-operation-name"

        // The body of this function looks absolutely incredible (/neg) because I have been
        // finding creative ways around the F# single-point-of-exit rule.

        let matchRequestMethods =
            lazy
                if _requestMethods.Count > 0 && not (_requestMethods.Contains request.Method) then
                    let methods = String.Join(", ", _requestMethods)

                    MockRequestMatchResult(
                        false,
                        $"The request method '{request.Method}' is not matched with {methods}"
                    )
                    |> Task.FromResult
                    |> Some
                else
                    None

        let matchActionNames =
            lazy
                if _actionNames.Count > 0 then
                    if not (request.Headers.Contains headerActionName) then
                        MockRequestMatchResult(
                            false,
                            $"The action name header '{headerActionName}' does not exist in the request so matching has failed - the header is only generated by the Logic Apps runtime when the 'Suppress workflow headers' setting for the action is off"
                        )
                        |> Task.FromResult
                        |> Some
                    else
                        let actualRequestHeaderValue =
                            request.Headers.GetValues(headerActionName)
                            |> Seq.tryHead
                            |> Option.defaultValue ""

                        if not (_actionNames.Contains actualRequestHeaderValue) then
                            let actionNames = String.Join(", ", _actionNames |> Seq.map (fun x -> $"'{x}'"))

                            MockRequestMatchResult(
                                false,
                                $"The action name '{actualRequestHeaderValue}' is not matched with {actionNames}"
                            )
                            |> Task.FromResult
                            |> Some
                        else
                            None
                else
                    None

        let matchPaths =
            lazy
                if _requestPaths.Count > 0 then
                    let pathMatch =
                        _requestPaths
                        |> Seq.exists (fun path ->
                            match path.MatchType with
                            | PathMatchType.Exact -> request.RequestUri.AbsolutePath = path.Path
                            | PathMatchType.Contains -> request.RequestUri.AbsolutePath.Contains path.Path
                            | PathMatchType.EndsWith -> request.RequestUri.AbsolutePath.EndsWith path.Path
                            | _ -> false)

                    if not pathMatch then
                        MockRequestMatchResult(
                            false,
                            $"The request absolute path '{request.RequestUri.AbsolutePath}' is not matched"
                        )
                        |> Task.FromResult
                        |> Some
                    else
                        None
                else
                    None

        let matchRequestHeaders =
            lazy
                if _requestHeaders.Count > 0 then
                    if request.Headers |> Seq.isEmpty then
                        MockRequestMatchResult(false, "The request does not have any headers so matching has failed")
                        |> Task.FromResult
                        |> Some
                    else
                        _requestHeaders
                        |> Seq.tryPick (fun requestHeader ->
                            if not (request.Headers.Contains requestHeader.Key) then
                                Some $"The request does not contain a header named '{requestHeader.Key}'"
                            else
                                let actualRequestHeaderValue =
                                    request.Headers.GetValues(requestHeader.Key)
                                    |> Seq.tryHead
                                    |> Option.defaultValue ""

                                if requestHeader.Value <> null && requestHeader.Value <> actualRequestHeaderValue then
                                    Some
                                        $"The request contains a header named '{requestHeader.Key}' but the value is '{actualRequestHeaderValue}' and the test is expecting a value of '{requestHeader.Value}'"
                                else
                                    None)
                        |> Option.map (fun message -> MockRequestMatchResult(false, message) |> Task.FromResult)
                else
                    None

        let matchQueryParams =
            lazy
                if _requestQueryParams.Count > 0 then
                    let parsedParamsAsDictionary =
                        InternalHelper.parseQueryString request.RequestUri.Query

                    if parsedParamsAsDictionary.Count = 0 then
                        MockRequestMatchResult(
                            false,
                            "The request does not have any query parameters so matching has failed"
                        )
                        |> Task.FromResult
                        |> Some
                    else
                        _requestQueryParams
                        |> Seq.tryPick (fun requestParam ->
                            match parsedParamsAsDictionary |> OrderedMap.tryFind requestParam.Key with
                            | None -> Some $"The request does not contain a query parameter named '{requestParam.Key}'"
                            | Some actualRequestParamValue when
                                requestParam.Value <> null && requestParam.Value <> actualRequestParamValue
                                ->
                                Some
                                    $"The request contains a query parameter named '{requestParam.Key}' but the value is '{actualRequestParamValue}' and the test is expecting a value of '{requestParam.Value}'"
                            | _ -> None)
                        |> Option.map (fun message -> MockRequestMatchResult(false, message) |> Task.FromResult)
                else
                    None

        if matchRequestMethods.Value.IsSome then
            matchRequestMethods.Value.Value
        elif matchActionNames.Value.IsSome then
            matchActionNames.Value.Value
        elif matchPaths.Value.IsSome then
            matchPaths.Value.Value
        elif matchRequestHeaders.Value.IsSome then
            matchRequestHeaders.Value.Value
        elif matchQueryParams.Value.IsSome then
            matchQueryParams.Value.Value
        elif
            _requestContentTypes.Count > 0
            && not (_requestContentTypes.Contains(request.Content.Headers.ContentType.ToString()))
        then
            let contentTypes =
                String.Join(", ", _requestContentTypes |> Seq.map (fun x -> $"'{x}'"))

            MockRequestMatchResult(
                false,
                $"The request content type '{request.Content.Headers.ContentType}' is not matched with {contentTypes}"
            )
            |> Task.FromResult
        else
            task {
                let! contentStringMatchFail =
                    task {
                        if _requestContentStringMatcherDelegate <> null then
                            let! contentAsString = requestCache.ContentAsStringAsync()

                            if not (_requestContentStringMatcherDelegate.Invoke contentAsString) then
                                return MockRequestMatchResult(false, "The request content is not matched") |> Some
                            else
                                return None
                        else
                            return None
                    }

                if contentStringMatchFail.IsSome then
                    return contentStringMatchFail.Value
                else
                    let! contentJsonMatchFail =
                        task {
                            if _requestContentJsonMatcherDelegate <> null then
                                let! contentAsJson = requestCache.ContentAsJsonAsync()

                                if not (_requestContentJsonMatcherDelegate.Invoke contentAsJson) then
                                    return
                                        MockRequestMatchResult(false, "The JSON request content is not matched") |> Some
                                else
                                    return None
                            else
                                return None
                        }

                    if contentJsonMatchFail.IsSome then
                        return contentJsonMatchFail.Value
                    else
                        _requestMatchCounter <- _requestMatchCounter + 1

                        if
                            _requestMatchCounts.Count > 0
                            && not (_requestMatchCounts.Contains _requestMatchCounter)
                        then
                            let matchCounts = String.Join(", ", _requestMatchCounts)

                            return
                                MockRequestMatchResult(
                                    false,
                                    $"The current request match count is {_requestMatchCounter} which is not matched with {matchCounts}"
                                )
                        elif
                            _requestMatchCountsNot.Count > 0
                            && _requestMatchCountsNot.Contains _requestMatchCounter
                        then
                            let matchCountsNot = String.Join(", ", _requestMatchCountsNot)

                            return
                                MockRequestMatchResult(
                                    false,
                                    $"The current request match count is {_requestMatchCounter} which is not matched with NOT {matchCountsNot}"
                                )
                        else
                            return MockRequestMatchResult(true)
            }

type MockRequestLog() =
    inherit MockRequest()

    member val Log = List<string>() with get, set

type internal MockDefinition
    (writeMockRequestMatchingLogs: bool, defaultHttpResponseStatusCode: int, mockResponsesFromBase: List<MockResponse>)
    =
    do
        if not (Enum.IsDefined(typeof<HttpStatusCode>, defaultHttpResponseStatusCode)) then
            raise (
                TestException
                    $"The default HTTP response status code of {defaultHttpResponseStatusCode} is not a valid status code."
            )

    let _writeMockRequestMatchingLogs = writeMockRequestMatchingLogs

    let _defaultHttpResponseStatusCode =
        enum<HttpStatusCode> defaultHttpResponseStatusCode

    let mutable _mockResponses: List<MockResponse> = null
    let _mockResponsesFromBase = mockResponsesFromBase
    let _mockResponsesFromTestCase = List<MockResponse>()

    let mutable _mockResponseDelegate: Func<HttpRequestMessage, HttpResponseMessage> =
        null

    let _mockRequestLog = ConcurrentBag<MockRequestLog>()
    let mutable _mockRequestsAsList: List<MockRequest> = null

    static member internal Random = Random.Shared

    member _.MockResponseDelegate
        with set value = _mockResponseDelegate <- value

    member this.AddMockResponse(mockRequestMatcher: IMockRequestMatcher) : IMockResponse =
        this.AddMockResponse(null, mockRequestMatcher)

    member this.AddMockResponse(name: string, mockRequestMatcher: IMockRequestMatcher) : IMockResponse =
        if
            not (String.IsNullOrEmpty name)
            && _mockResponsesFromTestCase |> Seq.exists (fun x -> x.MockName = name)
        then
            raise (
                ArgumentException $"A mock response with the name '{name}' has already been created in the test case."
            )

        if
            not (String.IsNullOrEmpty name)
            && _mockResponsesFromBase |> Seq.exists (fun x -> x.MockName = name)
        then
            raise (ArgumentException $"A mock response with the name '{name}' has already been created.")

        let mockResponse = MockResponse(name, mockRequestMatcher)
        _mockResponsesFromTestCase.Add mockResponse
        mockResponse

    member _.MockRequests = _mockRequestsAsList

    member this.TestRunStarting() : unit =
        _mockResponses <-
            Seq.append _mockResponsesFromTestCase _mockResponsesFromBase
            |> List<MockResponse>

        let msg =
            if _mockResponseDelegate = null then
                "No mock response delegate is configured"
            else
                "A mock response delegate is configured"

        Console.WriteLine(
            $"Using {_mockResponsesFromTestCase.Count} mock responses from the test case and {_mockResponsesFromBase.Count} mock responses from the test class. {msg}."
        )

    member this.TestRunComplete() : unit =
        _mockRequestsAsList <-
            _mockRequestLog
            |> Seq.map (fun x -> x :> MockRequest)
            |> Seq.sortBy _.Timestamp
            |> List<MockRequest>

        if _mockRequestLog.IsEmpty then
            Console.WriteLine("No mocked requests were logged")
        else
            Console.WriteLine("Mocked requests:")

            _mockRequestLog
            |> Seq.sortBy _.Timestamp
            |> Seq.iter (fun req ->
                let formattedTimestamp = req.Timestamp.ToString("HH:mm:ss.fff")
                Console.WriteLine($"    {formattedTimestamp}: {req.Method} {req.RequestUri.AbsoluteUri}")

                if _writeMockRequestMatchingLogs && req.Log.Count > 0 then
                    Console.WriteLine("    Mocked request matching logs:")
                    req.Log |> Seq.iter (fun s -> Console.WriteLine("      " + s))

                Console.WriteLine())

    member this.MatchRequestAndBuildResponseAsync(request: HttpRequestMessage) : HttpResponseMessage Task =
        ArgumentNullException.ThrowIfNull(request, nameof request)

        task {
            let! requestContent = request.Content.ReadAsStringAsync()

            let requestLog =
                MockRequestLog(
                    RequestUri = request.RequestUri,
                    Method = request.Method,
                    Headers = MockDefinition.CopyHeaders(request.Headers),
                    Content = requestContent,
                    ContentHeaders = MockDefinition.CopyHeaders(request.Content.Headers)
                )

            _mockRequestLog.Add requestLog

            let! fluentResponse =
                task {
                    if _mockResponses.Count > 0 then
                        requestLog.Log.Add $"Checking {_mockResponses.Count} mock request matchers:"
                        let! fluentResponse = this.GetResponseUsingFluentMocksAsync(request, requestLog.Log)

                        if fluentResponse <> null then
                            return Some fluentResponse
                        else
                            return None
                    else
                        requestLog.Log.Add "No mock request matchers have been configured"
                        return None
                }

            match fluentResponse with
            | Some response -> return response
            | None ->
                let delegateResponse =
                    if _mockResponseDelegate <> null then
                        requestLog.Log.Add "Running mock response delegate:"
                        let delegateResponse = this.GetResponseUsingDelegate(request, requestLog.Log)

                        if delegateResponse <> null then
                            Some delegateResponse
                        else
                            None
                    else
                        requestLog.Log.Add "No mock request delegate has been configured"
                        None

                match delegateResponse with
                | Some response -> return response
                | None ->
                    requestLog.Log.Add
                        $"Using default response: status code = {int _defaultHttpResponseStatusCode} ({_defaultHttpResponseStatusCode})"

                    return new HttpResponseMessage(_defaultHttpResponseStatusCode)
        }

    member private this.GetResponseUsingFluentMocksAsync
        (request: HttpRequestMessage, requestMatchingLog: List<string>)
        : HttpResponseMessage Task =
        task {
            let mockRequestCache = MockRequestCache(request)
            let mutable matchedResponse: HttpResponseMessage = null
            let mutable count = 0

            for mockResp in _mockResponses do
                if matchedResponse = null then
                    count <- count + 1

                    let mockNameParentheses =
                        if String.IsNullOrEmpty mockResp.MockName then
                            ""
                        else
                            $" ({mockResp.MockName})"

                    requestMatchingLog.Add $"  Checking mock request matcher #{count}{mockNameParentheses}:"

                    try
                        let! ourResponse =
                            mockResp.MatchRequestAndCreateResponseAsync(request, mockRequestCache, requestMatchingLog)

                        matchedResponse <- ourResponse
                    with ex ->
                        requestMatchingLog.Add($"    EXCEPTION: {ex.Message}")
                        ExceptionDispatchInfo.Capture(ex).Throw() // because reraise() is not available in a task

            return matchedResponse
        }

    member private this.GetResponseUsingDelegate
        (request: HttpRequestMessage, requestMatchingLog: List<string>)
        : HttpResponseMessage =
        try
            let matchedResponse = _mockResponseDelegate.Invoke(request)

            if matchedResponse = null then
                requestMatchingLog.Add("  Not matched")
            else
                requestMatchingLog.Add("  Matched")

            matchedResponse
        with ex ->
            requestMatchingLog.Add($"  EXCEPTION: {ex.Message}")
            reraise ()

    static member private CopyHeaders(headerCollection: HttpHeaders) =
        if headerCollection = null then
            null
        else
            let col = Dictionary<string, IEnumerable<string>>()

            headerCollection |> Seq.iter (fun header -> col.Add(header.Key, header.Value))

            col

and [<AllowNullLiteral>] MockResponseBuilder private () =
    let mutable _statusCode = HttpStatusCode.OK
    let _responseHeaders = Dictionary<string, string>()
    let mutable _delayDelegate: Func<TimeSpan> = null
    let mutable _contentDelegate = Func<HttpContent>(fun () -> null)
    let mutable _exceptionToThrow: Exception = null

    static member Create() = MockResponseBuilder()

    member this.WithStatusCode(statusCode) =
        _statusCode <- statusCode
        this

    member this.AfterDelay(delay: Func<TimeSpan>) =
        ArgumentNullException.ThrowIfNull(delay, nameof delay)

        _delayDelegate <- delay
        this

    member this.WithContent(content: Func<HttpContent>) =
        ArgumentNullException.ThrowIfNull(content, nameof content)

        _contentDelegate <- content
        this

    member this.WithSuccess() = this.WithStatusCode(HttpStatusCode.OK)

    member this.WithAccepted() =
        this.WithStatusCode(HttpStatusCode.Accepted)

    member this.WithNoContent() =
        this.WithStatusCode(HttpStatusCode.NoContent)

    member this.WithUnauthorized() =
        this.WithStatusCode(HttpStatusCode.Unauthorized)

    member this.WithNotFound() =
        this.WithStatusCode(HttpStatusCode.NotFound)

    member this.WithInternalServerError() =
        this.WithStatusCode(HttpStatusCode.InternalServerError)

    member this.WithStatusCode(statusCode) = this.WithStatusCode(statusCode)

    member this.WithHeader(name, value) =
        ArgumentNullException.ThrowIfNullOrEmpty(name, nameof name)
        ArgumentNullException.ThrowIfNullOrEmpty(value, nameof value)

        _responseHeaders.[name] <- value
        this

    member this.AfterDelay(secondsDelay) =
        this.AfterDelay(Func<TimeSpan>(fun () -> TimeSpan.FromSeconds(float secondsDelay)))

    member this.AfterDelay(delay) =
        this.AfterDelay(Func<TimeSpan>(fun () -> delay))

    member this.AfterDelay(secondsMin, secondsMax) =
        if secondsMax <= secondsMin then
            raise (ArgumentException("The 'min' seconds must be less than the 'max' seconds.", nameof secondsMin))

        this.AfterDelay(
            Func<TimeSpan>(fun () -> TimeSpan.FromSeconds(MockDefinition.Random.Next(secondsMin, secondsMax)))
        )

    member this.AfterDelay(min: TimeSpan, max) =
        if max <= min then
            raise (ArgumentException("The 'min' timespan must be less than the 'max' timespan.", nameof min))

        this.AfterDelay(
            Func<TimeSpan>(fun () ->
                TimeSpan.FromMilliseconds(
                    MockDefinition.Random.Next(int min.TotalMilliseconds, int max.TotalMilliseconds)
                ))
        )

    member this.WithContent(content) = this.WithContent(content)

    member this.WithContentAsJson(jsonString: string) =
        this.WithContent(Func<HttpContent>(fun () -> ContentHelper.CreateJsonStringContent(jsonString)))

    member this.WithContentAsJson(jsonStream: Stream) =
        this.WithContent(Func<HttpContent>(fun () -> ContentHelper.CreateJsonStreamContent(jsonStream)))

    member this.WithContentAsJson(body: obj) =
        this.WithContent(Func<HttpContent>(fun () -> ContentHelper.CreateJsonStringContent(body)))

    member this.WithContentAsJson(resourceName, containingAssembly) =
        this.WithContent(
            Func<HttpContent>(fun () ->
                ContentHelper.CreateJsonStreamContent(
                    ResourceHelper.GetAssemblyResourceAsStream(resourceName, containingAssembly)
                ))
        )

    member this.WithContentAsPlainText(value) =
        this.WithContent(Func<HttpContent>(fun () -> ContentHelper.CreatePlainStringContent(value)))

    member this.WithContentAsPlainText(stream) =
        this.WithContent(Func<HttpContent>(fun () -> ContentHelper.CreatePlainStreamContent(stream)))

    member this.WithContentAsPlainText(resourceName, containingAssembly) =
        this.WithContent(
            Func<HttpContent>(fun () ->
                ContentHelper.CreatePlainStreamContent(
                    ResourceHelper.GetAssemblyResourceAsStream(resourceName, containingAssembly)
                ))
        )

    member this.WithContent(value, contentType, encoding) =
        this.WithContent(Func<HttpContent>(fun () -> ContentHelper.CreateStringContent(value, contentType, encoding)))

    member this.WithContent(stream, contentType) =
        this.WithContent(Func<HttpContent>(fun () -> ContentHelper.CreateStreamContent(stream, contentType)))

    member this.WithContent(resourceName, containingAssembly, contentType) =
        this.WithContent(
            Func<HttpContent>(fun () ->
                ContentHelper.CreateStreamContent(
                    ResourceHelper.GetAssemblyResourceAsStream(resourceName, containingAssembly),
                    contentType
                ))
        )

    member this.ThrowsException(exceptionToThrow: Exception) =
        ArgumentNullException.ThrowIfNull(exceptionToThrow, nameof exceptionToThrow)
        _exceptionToThrow <- exceptionToThrow
        this

    interface IMockResponseBuilder with
        member this.WithSuccess() = this.WithSuccess()
        member this.WithAccepted() = this.WithAccepted()
        member this.WithNoContent() = this.WithNoContent()
        member this.WithUnauthorized() = this.WithUnauthorized()
        member this.WithNotFound() = this.WithNotFound()
        member this.WithInternalServerError() = this.WithInternalServerError()
        member this.WithStatusCode(statusCode) = this.WithStatusCode(statusCode)
        member this.WithHeader(name, value) = this.WithHeader(name, value)
        member this.AfterDelay(secondsDelay: int) : IMockResponseBuilder = this.AfterDelay(secondsDelay)
        member this.AfterDelay(delay: TimeSpan) : IMockResponseBuilder = this.AfterDelay(delay)

        member this.AfterDelay(secondsMin: int, secondsMax: int) : IMockResponseBuilder =
            this.AfterDelay(secondsMin, secondsMax)

        member this.AfterDelay(min: TimeSpan, max: TimeSpan) : IMockResponseBuilder = this.AfterDelay(min, max)
        member this.WithContent(content: Func<HttpContent>) : IMockResponseBuilder = this.WithContent(content)
        member this.WithContentAsJson(jsonString: string) : IMockResponseBuilder = this.WithContentAsJson(jsonString)
        member this.WithContentAsJson(jsonStream: Stream) : IMockResponseBuilder = this.WithContentAsJson(jsonStream)
        member this.WithContentAsJson(body: obj) : IMockResponseBuilder = this.WithContentAsJson(body)

        member this.WithContentAsJson(resourceName, containingAssembly) =
            this.WithContentAsJson(resourceName, containingAssembly)

        member this.WithContentAsPlainText(value: string) : IMockResponseBuilder = this.WithContentAsPlainText(value)
        member this.WithContentAsPlainText(stream: Stream) : IMockResponseBuilder = this.WithContentAsPlainText(stream)

        member this.WithContentAsPlainText(resourceName, containingAssembly) =
            this.WithContentAsPlainText(resourceName, containingAssembly)

        member this.WithContent(value: string, contentType: string, encoding: Encoding) : IMockResponseBuilder =
            this.WithContent(value, contentType, encoding)

        member this.WithContent(stream: Stream, contentType: string) : IMockResponseBuilder =
            this.WithContent(stream, contentType)

        member this.WithContent
            (resourceName: string, containingAssembly: Assembly, contentType: string)
            : IMockResponseBuilder =
            this.WithContent(resourceName, containingAssembly, contentType)

        member this.ThrowsException(exceptionToThrow) = this.ThrowsException(exceptionToThrow)

    member internal this.BuildResponse(request: HttpRequestMessage) =
        if _exceptionToThrow <> null then
            raise _exceptionToThrow

        let response =
            new HttpResponseMessage(_statusCode, RequestMessage = request, Content = _contentDelegate.Invoke())

        _responseHeaders
        |> Seq.iter (fun header -> response.Headers.Add(header.Key, header.Value))

        response

    member internal this.ExecuteDelayAsync(requestMatchingLog: List<string>) : Task =
        match _delayDelegate with
        | null -> Task.CompletedTask
        | _ ->
            let delay = _delayDelegate.Invoke()

            requestMatchingLog.Add $"    Delaying response by {delay.TotalSeconds} seconds"
            Task.Delay(delay)

and MockResponse internal (name: string, mockRequestMatcher: IMockRequestMatcher) =
    do ArgumentNullException.ThrowIfNull(mockRequestMatcher, nameof mockRequestMatcher)

    let _mockName = name
    let _mockRequestMatcher = mockRequestMatcher :?> MockRequestMatcher
    let mutable _mockResponseBuilder: MockResponseBuilder = null

    member internal _.MockName = _mockName

    member this.RespondWith(mockResponseBuilder: IMockResponseBuilder) =
        ArgumentNullException.ThrowIfNull(mockResponseBuilder, nameof mockResponseBuilder)
        _mockResponseBuilder <- mockResponseBuilder :?> MockResponseBuilder

    member this.RespondWithDefault() =
        _mockResponseBuilder <- MockResponseBuilder.Create()

    interface IMockResponse with
        member this.RespondWith(mockResponseBuilder) = this.RespondWith(mockResponseBuilder)
        member this.RespondWithDefault() = this.RespondWithDefault()

    member internal this.MatchRequestAndCreateResponseAsync
        (request: HttpRequestMessage, requestCache: MockRequestCache, requestMatchingLog: List<string>)
        : HttpResponseMessage Task =
        ArgumentNullException.ThrowIfNull(request, nameof request)
        ArgumentNullException.ThrowIfNull(requestCache, nameof requestCache)

        if _mockRequestMatcher = null then
            raise (TestException "A request matcher has not been configured")

        if _mockResponseBuilder = null then
            raise (
                TestException
                    "A response builder has not been configured - use RespondWith() to create a response, or RespondWithDefault() to create a default response using a status code of 200 (OK) and no content"
            )

        task {
            let! matchResult = _mockRequestMatcher.MatchRequestAsync(request, requestCache)

            if matchResult.IsMatch then
                requestMatchingLog.Add "    Matched"
                do! _mockResponseBuilder.ExecuteDelayAsync(requestMatchingLog)
                return _mockResponseBuilder.BuildResponse(request)
            else
                requestMatchingLog.Add $"    Not matched - {matchResult.MatchLog}"
                return null
        }
