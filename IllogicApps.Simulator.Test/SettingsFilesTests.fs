module IllogicApps.Simulator.Test.SettingsFilesTest

open IllogicApps.Core.LogicAppActionSupport
open IllogicApps.Json
open NUnit.Framework
open Swensen.Unquote
open IllogicApps.Simulator
open Parameters
open ReadSupportingData

let testAppSettings =
    """
    {
      "IsEncrypted": false,
      "Values": {
        "AzureWebJobsStorage": "UseDevelopmentStorage=true",
        "AzureWebJobsSecretStorageType": "Files",
        "APP_KIND": "workflowapp",
        "FUNCTIONS_WORKER_RUNTIME": "node",
        "WORKFLOWS_SUBSCRIPTION_ID": "",
        "keyVault_VaultUri": "http://localhost",
        "keyVault_11_VaultUri": "http://localhost",
        "test:colon": "colon",
        "test__dunder": "dunder",
        "test___trunder": "trunder",
        "test____quunder": "quunder"
      }
    }
    """

let parsedAppSettings =
    lazy trap <@ appSettingsOfJson <| Parser.parse testAppSettings @>

[<Test>]
let ``Test got expected local settings`` () =
    let (Lazy data) = parsedAppSettings

    test
        <@
            data.ContainsKey "test:colon"
            && data.ContainsKey "test:dunder"
            && data.ContainsKey "test:_trunder"
            && data.ContainsKey "test::quunder"
        @>

[<Test>]
let ``Test cannot parse nested app configs`` () =
    let testBrokenAppSettings =
        """
    {
      "IsEncrypted": false,
      "Values": {
        "nest": {
          "test": "pest"
        }
      }
    }
    """

    let parsed = trap <@ Parser.parse testBrokenAppSettings @>
    raises <@ appSettingsOfJson parsed @>

let testParameters =
    """
    {
      "Testing": {
        "type": "Object",
        "value": {
          "1": 2,
          "wow": "cool"
        }
      },
      "AppSettingTest": {
        "type": "String",
        "value": "@appsetting('test:_trunder')"
      },
      "FloatTest": {
        "type": "Float",
        "value": 123.0
      }
    }"""

let parsedTestParameters =
    lazy trap <@ parametersOfJson <| Parser.parse testParameters @>

let valTesting =
    { type_ = VariableType.Object
      value = Conversions.createObject [ "1", Integer 2L; "wow", String "cool" ] }

let valAppSettingTest =
    { type_ = VariableType.String
      value = String "@appsetting('test:_trunder')" }

let valFloatTest =
    { type_ = VariableType.Float
      value = Float 123.0 }

[<Test>]
let ``Test got expected parameters`` () =
    let (Lazy data) = parsedTestParameters

    test
        <@
            OrderedMap.find "Testing" data = valTesting
            && OrderedMap.find "AppSettingTest" data = valAppSettingTest
            && OrderedMap.find "FloatTest" data = valFloatTest
        @>

[<Test>]
let ``Test that integer type is called int`` () =
    let data =
        """
        {
          "IntsAreCalledIntTest": {
            "type": "Int",
            "value": 123
          }
        }
        """

    let data = trap <@ parametersOfJson <| Parser.parse data @>

    let valIntsAreCalledIntTest =
        { type_ = VariableType.Integer
          value = Integer 123L }

    test <@ OrderedMap.find "IntsAreCalledIntTest" data = valIntsAreCalledIntTest @>

[<Test>]
let ``Test that integer type is not called integer`` () =
    let data =
        """
        {
          "IntsAreCalledIntTest": {
            "type": "Integer",
            "value": 123
          }
        }
        """

    let data = trap <@ Parser.parse data @>
    raises <@ parametersOfJson data @>

[<Test>]
let ``Test that parameters is almost completely case insensitive`` () =
    let messyTestParameters =
        """
        {
          "Testing": {
            "tyPe": "object",
            "value": {
              "1": 2,
              "wow": "cool"
            }
          },
          "AppSettingTest": {
            "TYPE": "STRING",
            "vaLUE": "@appsetting('test:_trunder')"
          },
          "FloatTest": {
            "tYpE": "fLOAT",
            "vAlUe": 123.0
          }
        }"""

    let parsed = trap <@ parametersOfJson <| Parser.parse messyTestParameters @>

    test
        <@
            OrderedMap.find "Testing" parsed = valTesting
            && OrderedMap.find "AppSettingTest" parsed = valAppSettingTest
            && OrderedMap.find "FloatTest" parsed = valFloatTest
        @>

[<Test>]
let ``Test that parameters does not accept null even as object`` () =
    let data =
        """
        {
          "NullTest": {
            "type": "Object",
            "value": null
          }
        }
        """

    let data = trap <@ Parser.parse data @>
    raises <@ parametersOfJson data @>

[<Test>]
let ``Test that parameters does not accept missing values`` () =
    let data =
        """
        {
          "MissingTest": {
            "type": "Object"
          }
        }
        """

    let data = trap <@ Parser.parse data @>
    raises <@ parametersOfJson data @>

[<Test>]
let ``Test that parameters does not upcast int to float`` () =
    let data =
        """
        {
          "IntToFloatTest": {
            "type": "Float",
            "value": 123
          }
        }
        """

    let data = trap <@ Parser.parse data @>
    raises <@ parametersOfJson data @>

[<Test>]
let ``Test parameter value retrieval from simulator`` () =
    let (Lazy appConfig) = parsedAppSettings
    let (Lazy parameters) = parsedTestParameters

    let sim =
        Simulator.CreateUntriggered
            { SimulatorCreationOptions.dummy with
                appConfig = appConfig
                parameters = parameters }

    test
        <@
            sim.GetParameter "Testing" = Some valTesting.value
            && sim.GetParameter "AppSettingTest" = Some(String "trunder")
            && sim.GetParameter "FloatTest" = Some valFloatTest.value
        @>

[<Test>]
let ``Test that parameters cannot use any function other than appsetting`` () =
    let data =
        """
        {
          "BadFunctionTest": {
            "type": "String",
            "value": "@add(1,2)"
          }
        }
        """

    let data = trap <@ parametersOfJson <| Parser.parse data @>

    let sim =
        Simulator.CreateUntriggered
            { SimulatorCreationOptions.dummy with
                parameters = data }

    raises <@ sim.GetParameter "BadFunctionTest" @>

[<Test>]
let ``Test that appsetting parameters which evaluate to null are valid`` () =
    let (Lazy parameters) = parsedTestParameters

    let sim =
        Simulator.CreateUntriggered
            { SimulatorCreationOptions.dummy with
                parameters = parameters }

    test <@ sim.GetParameter "AppSettingTest" = Some Null @>
