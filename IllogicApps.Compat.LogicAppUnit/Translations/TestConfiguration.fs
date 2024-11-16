// NOTICE: This file is a translation of an equivalent file in the LogicAppUnit project, and as such is licensed
// under the same terms as that project. The license can be found in this project's directory.
namespace LogicAppUnit

open System.Collections.Generic

type TestConfigurationWorkflow() =
    member val ExternalApiUrlsToMock = List<string>() with get, set
    member val BuiltInConnectorsToMock = List<string>() with get, set
    member val ManagedApisToMock = List<string>() with get, set
    member val AutoConfigureWithStatelessRunHistory = true with get, set
    member val RemoveHttpRetryConfiguration = true with get, set
    member val RemoveHttpChunkingConfiguration = true with get, set
    member val RemoveManagedApiConnectionRetryConfiguration = true with get, set

type TestConfigurationAzurite() =
    member val EnableAzuritePortCheck = true with get, set
    member val BlobServicePort = 10000 with get, set
    member val QueueServicePort = 10001 with get, set
    member val TableServicePort = 10002 with get, set

type TestConfigurationRunner() =
    member val MaxWorkflowExecutionDuration = 300 with get, set
    member val DefaultHttpResponseStatusCode = 200 with get, set

type TestConfigurationLogging() =
    member val WriteFunctionRuntimeStartupLogs = false with get, set
    member val WriteMockRequestMatchingLogs = false with get, set

type TestConfiguration() =
    member val LocalSettingsFilename: string = null with get, set
    member val Azurite = TestConfigurationAzurite() with get, set
    member val Logging = TestConfigurationLogging() with get, set
    member val Runner = TestConfigurationRunner() with get, set
    member val Workflow = TestConfigurationWorkflow() with get, set
