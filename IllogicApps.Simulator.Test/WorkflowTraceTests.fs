module IllogicApps.Simulator.Test.WorkflowTraceTests

open System
open System.Collections.Generic
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Reflection
open System.Text
open DEdge.Diffract
open IllogicApps.Core.ExternalServiceTypes
open IllogicApps.Core.HttpModel.HttpParsing
open IllogicApps.Core.HttpModel.HttpWriting
open NUnit.Framework
open Swensen.Unquote

open IllogicApps.Core
open IllogicApps.Core.CompletedStepTypes
open IllogicApps.Json
open IllogicApps.Simulator

// To generate a trace, run:
// curl -s -g --trace trace-whatever.txt 'http://your-trigger-url'
// plus any extra parameters you might want like -XPOST

let decodeHexdumpLine (line: ReadOnlySpan<char>) : byte array =
    let hexChars = line.Slice(6, 48).ToString() // in a just world I would not need to say .ToString()
    let length = [ 16..-1..1 ] |> Seq.find (fun i -> hexChars[i * 3 - 2] <> ' ')
    Array.init length (fun i -> Byte.Parse(hexChars.AsSpan(i * 3, 2), System.Globalization.NumberStyles.HexNumber))

type CurlTraceParseState =
    | RequestHeader
    | RequestBody
    | ResponseHeader
    | ResponseBody

[<RequireQualifiedAccess>]
type CurlTraceIntermediate =
    { state: CurlTraceParseState
      requestHeader: byte array list
      requestBody: byte array list
      responseHeader: byte array list
      responseBody: byte array list }

    static member Empty =
        { state = RequestHeader
          requestHeader = []
          requestBody = []
          responseHeader = []
          responseBody = [] }

type CurlTraceRaw =
    { requestHeader: byte array
      requestBody: byte array
      responseHeader: byte array
      responseBody: byte array }

let readRawCurlTrace (trace: Stream) =
    use reader = new StreamReader(trace)

    Seq.unfold (fun _ -> reader.ReadLine() |> Option.ofObj |> Option.map (fun l -> l, ())) ()
    |> Seq.filter (fun l -> l.Length > 8 && (l.[0] <> '=' || l.[1] <> '='))
    |> Seq.fold
        (fun (acc: CurlTraceIntermediate) el ->
            match el.[0], el[8] with
            | '=', 'h' -> { acc with state = RequestHeader }
            | '=', 'd' -> { acc with state = RequestBody }
            | '<', 'h' -> { acc with state = ResponseHeader }
            | '<', 'd' -> { acc with state = ResponseBody }
            | _ ->
                let line = decodeHexdumpLine (el.AsSpan())

                match acc.state with
                | RequestHeader ->
                    { acc with
                        requestHeader = line :: acc.requestHeader }
                | RequestBody ->
                    { acc with
                        requestBody = line :: acc.requestBody }
                | ResponseHeader ->
                    { acc with
                        responseHeader = line :: acc.responseHeader }
                | ResponseBody ->
                    { acc with
                        responseBody = line :: acc.responseBody })
        CurlTraceIntermediate.Empty
    |> fun acc ->
        { requestHeader = acc.requestHeader |> List.rev |> Seq.concat |> Array.ofSeq
          requestBody = acc.requestBody |> List.rev |> Seq.concat |> Array.ofSeq
          responseHeader = acc.responseHeader |> List.rev |> Seq.concat |> Array.ofSeq
          responseBody = acc.responseBody |> List.rev |> Seq.concat |> Array.ofSeq }

type CurlTraceRequest =
    { httpMethod: string
      relativePath: string
      httpVersion: string
      headers: (string * string) list
      body: byte array }

type CurlTraceResponse =
    { httpVersion: string
      statusCode: int
      statusMessage: string
      headers: (string * string) list
      body: byte array }

#nowarn "0025" // Incomplete pattern match; yes I know, it doesn't need to be robust

let parseCurlTrace (raw: CurlTraceRaw) : CurlTraceRequest * CurlTraceResponse =
    let requestHeader = Encoding.UTF8.GetString(raw.requestHeader)
    let responseHeader = Encoding.UTF8.GetString(raw.responseHeader)

    let requestLines =
        requestHeader.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)

    let responseLines =
        responseHeader.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)

    let [| requestMethod; relativePath; requestHttpVersion |] =
        requestLines.[0].Split([| ' ' |], 3)

    let [| responseHttpVersion; statusCode; statusMessage |] =
        responseLines.[0].Split([| ' ' |], 3)

    let parseHeaders (lines: string seq) =
        lines
        |> Seq.skip 1
        |> Seq.map (fun l ->
            let parts = l.Split([| ':' |], 2)
            parts.[0], parts.[1].Trim())
        |> List.ofSeq

    { httpMethod = requestMethod
      relativePath = relativePath
      httpVersion = requestHttpVersion
      headers = parseHeaders requestLines
      body = raw.requestBody },
    { httpVersion = responseHttpVersion
      statusCode = int statusCode
      statusMessage = statusMessage
      headers = parseHeaders responseLines
      body = raw.responseBody }

let assembly = Assembly.GetExecutingAssembly()

let readResource name =
    assembly.GetManifestResourceStream(name)

let readTestCase (workflowName, traceName) =
    let workflow = readResource (workflowName)
    let trace = readResource (traceName)

    let rawTrace = readRawCurlTrace trace
    let request, response = parseCurlTrace rawTrace

    let workflowName = request.relativePath.Split('/', 4)[2]

    let workflowJson =
        use workflowJson = new StreamReader(workflow)
        workflowJson.ReadToEnd()

    let logicApp = ReadLogicApp.decodeLogicApp workflowJson

    workflowName, logicApp, request, response

let namedTestCases names =
    let resources = assembly.GetManifestResourceNames()
    let assemblyName = assembly.GetName().Name

    let expectedPrefix = $"{assemblyName}.TestData."
    let expectedSuffix = ".workflow.json"

    resources
    |> Seq.choose (fun n ->
        if n.StartsWith(expectedPrefix) && n.EndsWith(expectedSuffix) then
            Some(n.Substring(expectedPrefix.Length, n.Length - expectedPrefix.Length - expectedSuffix.Length))
        else
            None)
    |> Seq.filter (fun name -> names |> List.contains name)
    |> Seq.collect (fun name ->
        let tracePrefix = $"{expectedPrefix}{name}.trace-"

        resources
        |> Seq.filter (fun n -> n.StartsWith(tracePrefix) && n.EndsWith(".txt"))
        |> Seq.map (fun traceName -> $"{expectedPrefix}{name}{expectedSuffix}", traceName))
    |> Seq.map TestCaseData
    |> List.ofSeq

let logicAppHostQueryKeys = [| "api-version"; "sp"; "sv"; "sig" |]

let ``Test cases for workflows that respond with trigger`` =
    namedTestCases [ "simpleRelativePath"; "simpleEcho" ]

[<TestCaseSource(nameof ``Test cases for workflows that respond with trigger``)>]
let ``Test IllogicApps output matches logic app trace for workflows that respond with trigger`` workflowName traceName =
    let workflowName, logicApp, request, expectedResponse =
        readTestCase (workflowName, traceName)

    let expectedResponseStr = Encoding.UTF8.GetString(expectedResponse.body)
    let parsedExpectedResponse = Parser.parse expectedResponseStr

    let relativePathPart =
        let invokePart = "/invoke/"
        let invocation = request.relativePath.IndexOf(invokePart, StringComparison.Ordinal)

        if invocation = -1 then
            ""
        else
            let query = request.relativePath.IndexOf('?', invocation)

            if query = -1 then
                request.relativePath.Substring(invocation + invokePart.Length)
            else
                request.relativePath.Substring(invocation + invokePart.Length, query - invocation - invokePart.Length)

    let query =
        let queryPart = request.relativePath.IndexOf('?', StringComparison.Ordinal)

        if queryPart = -1 then
            ""
        else
            request.relativePath.Substring(queryPart + 1)

    let parsedQuery =
        parseQueryString query
        |> OrderedMap.filter (fun k _ -> not (Array.contains k logicAppHostQueryKeys))
        |> fun m -> if m.Count = 0 then None else Some m

    // We can't reasonably be expected to match these without faking it:
    let workflowRunId =
        trap
            <@
                parsedExpectedResponse
                |> JsonTree.getKey "originHistoryName"
                |> Conversions.ensureString
            @>

    let clientTrackingId =
        parsedExpectedResponse
        |> JsonTree.getKey "clientTrackingId"
        |> Conversions.ensureString

    let trackingId =
        parsedExpectedResponse
        |> JsonTree.getKey "trackingId"
        |> Conversions.ensureString

    let startTime =
        parsedExpectedResponse
        |> JsonTree.getKey "startTime"
        |> Conversions.ensureString

    let endTime =
        parsedExpectedResponse |> JsonTree.getKey "endTime" |> Conversions.ensureString

    let workflowId =
        expectedResponse.headers
        |> List.pick (function
            | "x-ms-workflow-id", v -> Some v
            | _ -> None)

    let workflowVersion =
        expectedResponse.headers
        |> List.pick (function
            | "x-ms-workflow-version", v -> Some v
            | _ -> None)

    let triggerName, triggerAction =
        logicApp.definition.triggers |> OrderedMap.toSeq |> Seq.head

    let actualResponse = ref None

    let responseHandler _sim request =
        match request with
        | HttpResponse(resp) when actualResponse.Value = None ->
            actualResponse.Value <- Some resp
            true
        | _ -> false

    let triggerRequest =
        let headers =
            // Round-trip the headers via HttpRequestHeaders (which can only be instantiated by a HttpRequestMessage)
            // and sort the header keys from the Headers slot (but not the Content.Headers slot)
            // to ensure that they are ordered the same way the Logic Apps host orders them
            use requestContent = new ByteArrayContent(Array.empty)
            use requestMessage = new HttpRequestMessage(Content = requestContent)

            request.headers
            |> List.iter (fun (k, v) ->
                if not (requestMessage.Headers.TryAddWithoutValidation(k, v)) then
                    if not (requestMessage.Content.Headers.TryAddWithoutValidation(k, v)) then
                        failwithf "Don't know where to put the %s header" k)

            let sortedInitialHeaders =
                requestMessage.Headers |> Seq.sortBy (fun (KeyValue(k, _)) -> k)

            Seq.append sortedInitialHeaders requestMessage.Content.Headers
            |> Seq.map (fun (KeyValue(k, v)) -> KeyValuePair(k, v |> String.concat ","))
            |> OrderedMap.CreateRange

        let contentType = headers |> OrderedMap.tryFind "Content-Type"

        { method = HttpMethod.Parse request.httpMethod
          relativePath = relativePathPart
          queries = parsedQuery
          headers = Some headers
          body = decodeOptionalBodyByContentType contentType request.body }

    let simCreationOptions =
        { SimulatorCreationOptions.Default with
            workflowName = workflowName
            workflowId = workflowId
            workflowVersion = workflowVersion
            runId = workflowRunId
            originatingRunId = workflowRunId
            externalServiceHandlers = [ responseHandler ]
            triggerResult = Invoked triggerRequest
            isStateless = Simulator.workflowIsStateless logicApp }

    let sim = Simulator.CreateUntriggered simCreationOptions

    sim.ExecuteTrigger triggerName triggerAction

    sim.EditTrigger (function
        | Invoked _ -> failwith "Trigger was not run"
        | Completed t ->
            Completed
                { t with
                    startTime = startTime
                    endTime = endTime
                    clientTrackingId = clientTrackingId
                    trackingId = trackingId })

    sim.RunWholeWorkflow logicApp.definition.actions

    let actualResponse = trap <@ actualResponse.Value.Value @>
    let actualResponseBody = trap <@ actualResponse.body.Value @>

    // Add some synthetic headers to the response
    let tweakedHeaders =
        let builder = OrderedMap.Builder()

        match contentOfJson actualResponseBody with
        | Some(contentType, body) ->
            builder
                .Add("Content-Length", body.Length.ToString())
                .Add("Content-Type", contentType |> MediaTypeHeaderValue.Parse |> _.ToString())
            |> ignore
        | None -> ()

        builder
            .MaybeAdd(
                "Date",
                expectedResponse.headers
                |> List.tryPick (function
                    | "Date", v -> Some v
                    | _ -> None)
            )
            .Add("Server", "Kestrel")
            .AddRange(actualResponse.headers |> Option.defaultValue OrderedMap.empty)
            .Build()


    test <@ expectedResponse.statusCode = actualResponse.statusCode @>

    trap
        <@
            Differ.Assert(
                parsedExpectedResponse,
                actualResponseBody,
                param =
                    { Differ.AssertPrintParams with
                        neutralName = "Response body object" }
            )
        @>

    trap
        <@
            Differ.Assert(
                expectedResponseStr,
                Conversions.stringOfJson actualResponseBody,
                param =
                    { Differ.AssertPrintParams with
                        neutralName = "Response body string" }
            )
        @>

    trap
        <@
            Differ.Assert(
                expectedResponse.headers,
                tweakedHeaders |> OrderedMap.toList,
                param =
                    { Differ.AssertPrintParams with
                        neutralName = "Response headers" }
            )
        @>
