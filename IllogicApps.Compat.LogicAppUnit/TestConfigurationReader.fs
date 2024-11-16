module IllogicApps.Compat.LogicAppUnit.TestConfigurationReader

open System.Collections.Generic
open IllogicApps.Json
open LogicAppUnit

let inline private addAValue (field: List<string>) key json =
    json
    |> JsonTree.tryGetKeyCaseInsensitive key
    |> Option.iter (fun v -> field.AddRange(Conversions.stringListOfJson v))

let inline private setAValue ([<InlineIfLambda>] f) key json =
    json |> JsonTree.tryGetKeyCaseInsensitive key |> Option.iter f

let inline private jbool json = json |> Conversions.ensureBoolean

let inline private jint json =
    json |> Conversions.ensureInteger |> int

// None of this applies to IllogicApps but we will read them anyway.
// - We always mock everything
// - HTTP retry and chunking don't do anything because they're mocked
// - Stateful/stateless runs are not distinguished
let testConfigurationWorkflowOfJson json =
    let value = TestConfigurationWorkflow()
    json |> addAValue value.ExternalApiUrlsToMock "externalApiUrlsToMock"
    json |> addAValue value.BuiltInConnectorsToMock "builtInConnectorsToMock"
    json |> addAValue value.ManagedApisToMock "managedApisToMock"

    json
    |> setAValue (fun v -> value.AutoConfigureWithStatelessRunHistory <- jbool v) "autoConfigureWithStatelessRunHistory"

    json
    |> setAValue (fun v -> value.RemoveHttpRetryConfiguration <- jbool v) "removeHttpRetryConfiguration"

    json
    |> setAValue (fun v -> value.RemoveHttpChunkingConfiguration <- jbool v) "removeHttpChunkingConfiguration"

    json
    |> setAValue
        (fun v -> value.RemoveManagedApiConnectionRetryConfiguration <- jbool v)
        "removeManagedApiConnectionRetryConfiguration"

    value

// None of this applies to us
let testConfigurationAzuriteOfJson json =
    let value = TestConfigurationAzurite()

    json
    |> setAValue (fun v -> value.EnableAzuritePortCheck <- jbool v) "enableAzuritePortCheck"

    json |> setAValue (fun v -> value.BlobServicePort <- jint v) "blobServicePort"
    json |> setAValue (fun v -> value.QueueServicePort <- jint v) "queueServicePort"
    json |> setAValue (fun v -> value.TableServicePort <- jint v) "tableServicePort"

    value

// Oh hey these are useful
let testConfigurationRunnerOfJson json =
    let value = TestConfigurationRunner()

    json
    |> setAValue (fun v -> value.MaxWorkflowExecutionDuration <- jint v) "maxWorkflowExecutionDuration"

    json
    |> setAValue (fun v -> value.DefaultHttpResponseStatusCode <- jint v) "defaultHttpResponseStatusCode"

    value

// One out of two isn't bad: we don't use the function runtime
let testConfigurationLoggingOfJson json =
    let value = TestConfigurationLogging()

    json
    |> setAValue (fun v -> value.WriteFunctionRuntimeStartupLogs <- jbool v) "writeFunctionRuntimeStartupLogs"

    json
    |> setAValue (fun v -> value.WriteMockRequestMatchingLogs <- jbool v) "writeMockRequestMatchingLogs"

    value

// And of course we want the local settings filename
let testConfigurationOfJson json =
    let value = TestConfiguration()

    json
    |> setAValue (fun v -> value.LocalSettingsFilename <- Conversions.ensureString v) "localSettingsFilename"

    json
    |> setAValue (fun v -> value.Azurite <- testConfigurationAzuriteOfJson v) "azurite"

    json
    |> setAValue (fun v -> value.Logging <- testConfigurationLoggingOfJson v) "logging"

    json
    |> setAValue (fun v -> value.Runner <- testConfigurationRunnerOfJson v) "runner"

    json
    |> setAValue (fun v -> value.Workflow <- testConfigurationWorkflowOfJson v) "workflow"

    value

let readTestConfiguration filename =
    filename
    |> System.IO.File.ReadAllText
    |> Parser.parse
    |> testConfigurationOfJson
