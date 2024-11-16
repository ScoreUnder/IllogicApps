namespace LogicAppUnit

open System
open System.Collections.Generic
open System.IO
open IllogicApps.Compat.LogicAppUnit
open IllogicApps.Compat.LogicAppUnit.InternalHelper
open IllogicApps.Core
open IllogicApps.Json
open IllogicApps.Simulator
open IllogicApps.Simulator.Parameters
open LogicAppUnit.Mocking

type private InitialisedWorkflowTestBase =
    { workflows: (string * LogicAppSpec.Root) seq
      workflowName: string
      appSettings: OrderedMap<string, string>
      parameters: OrderedMap<string, Parameter>
      testConfig: TestConfiguration }

(*
The interface specified in this comment is work that was done by the original LogicAppUnit author,
and is licensed under the MIT license; see Translations/LICENSE_LogicAppUnit.md

Public-facing interface in LogicAppUnit:

public abstract class

zero-arg constructor
public IMockResponse AddMockResponse(IMockRequestMatcher mockRequestMatcher)
public IMockResponse AddMockResponse(string name, IMockRequestMatcher mockRequestMatcher)

protected static string MockTestWorkflowHostUri with get
protected void Initialize(string logicAppBasePath, string workflowName)
protected void Initialize(string logicAppBasePath, string workflowName, string localSettingsFilename)
protected static void Close()
protected ITestRunner CreateTestRunner()
protected ITestRunner CreateTestRunner(Dictionary<string, string> localSettingsOverrides)
*)
[<AbstractClass>]
type WorkflowTestBase() =
    [<Literal>]
    let WORKFLOW_FILENAME = "workflow.json"

    [<Literal>]
    let LOCAL_SETTINGS_FILENAME = "local.settings.json"

    [<Literal>]
    let PARAMETERS_FILENAME = "parameters.json"

    [<Literal>]
    let TEST_CONFIGURATION_FILENAME = "testConfiguration.json"

    let mutable mockResponses = []
    let mutable initialised = None

    let makeMockDefinition (testConfiguration: TestConfiguration) =
        MockDefinition(
            testConfiguration.Logging.WriteMockRequestMatchingLogs,
            testConfiguration.Runner.DefaultHttpResponseStatusCode,
            mockResponses |> List.rev |> List<MockResponse>
        )

    member this.AddMockResponse(mockRequestMatcher: IMockRequestMatcher) : IMockResponse =
        let resp = MockResponse(null, mockRequestMatcher)
        mockResponses <- resp :: mockResponses
        resp

    member this.AddMockResponse(name: string, mockRequestMatcher: IMockRequestMatcher) : IMockResponse =
        let resp = MockResponse(name, mockRequestMatcher)
        mockResponses <- resp :: mockResponses
        resp

    static member MockTestWorkflowHostUri = "http://mockHost.localhost"

    member this.Initialize(logicAppBasePath: string, workflowName: string) : unit =
        this.Initialize(logicAppBasePath, workflowName, null, Array.Empty<string>())

    member this.Initialize(logicAppBasePath: string, workflowName: string, localSettingsFilename: string) : unit =
        this.Initialize(logicAppBasePath, workflowName, localSettingsFilename, Array.Empty<string>())

    // Extended versions with support for workflow families
    member this.Initialize
        (logicAppBasePath: string, workflowName: string, [<ParamArray>] otherWorkflows: string array)
        : unit =
        this.Initialize(logicAppBasePath, workflowName, null, otherWorkflows)

    member this.Initialize
        (
            logicAppBasePath: string,
            workflowName: string,
            localSettingsFilename: string,
            [<ParamArray>] otherWorkflows: string array
        ) : unit =
        ArgumentNullException.ThrowIfNull(logicAppBasePath, nameof logicAppBasePath)
        ArgumentNullException.ThrowIfNullOrEmpty(workflowName, nameof workflowName)

        let testConfig =
            try
                TestConfigurationReader.readTestConfiguration TEST_CONFIGURATION_FILENAME
            with :? FileNotFoundException ->
                TestConfiguration()

        let localSettingsFilename =
            localSettingsFilename
            |> sanitiseNull testConfig.LocalSettingsFilename
            |> sanitiseNull LOCAL_SETTINGS_FILENAME

        let childWorkflows = otherWorkflows |> sanitiseNull Array.empty
        let allWorkflowNames = Seq.append [| workflowName |] childWorkflows

        let workflows =
            allWorkflowNames
            |> Seq.map (fun name ->
                name, (ReadLogicApp.readLogicApp (Path.Combine(logicAppBasePath, name, WORKFLOW_FILENAME))))
            |> Seq.toList

        let appSettings =
            Path.Combine(logicAppBasePath, localSettingsFilename)
            |> ReadSupportingData.readAppSettings

        let parameters =
            try
                Path.Combine(logicAppBasePath, PARAMETERS_FILENAME)
                |> ReadSupportingData.readParameters
            with :? FileNotFoundException ->
                OrderedMap.empty

        initialised <-
            Some
                { workflows = workflows
                  workflowName = workflowName
                  appSettings = appSettings
                  parameters = parameters
                  testConfig = testConfig }

    static member Close() : unit = ()

    member this.CreateTestRunner() : ITestRunner = this.CreateTestRunner(null)

    member this.CreateTestRunner(localSettingsOverrides: Dictionary<string, string>) : ITestRunner =
        match initialised with
        | None -> failwith "Not initialised"
        | Some settings ->
            let localSettingsOverrides =
                localSettingsOverrides |> sanitiseNull (Dictionary<string, string>())

            let localSettings =
                OrderedMap
                    .Builder()
                    .AddRange(settings.appSettings)
                    .SetRange(localSettingsOverrides)
                    .Build()

            new TestRunner(
                makeMockDefinition settings.testConfig,
                settings.workflows,
                settings.workflowName,
                localSettings,
                settings.parameters
            )
