module IllogicApps.Json.Test.Benchmarks

open System.Text.Json
open System.Text.Json.Nodes
open BenchmarkDotNet.Attributes
open IllogicApps.Json
open IllogicApps.Json.Conversions

type Dummy() =
    inherit obj()

let bigJson =
    lazy
        use stream =
            new System.IO.StreamReader(
                typeof<Dummy>.Assembly
                    .GetManifestResourceStream("IllogicApps.Json.Test.TestData.SkippingTestWorkflow.json")
            )

        stream.ReadToEnd()

type GetterBenchmarks() =
    let alreadyParsed = JsonParser.parse bigJson.Value

    let alreadyParsedSystemTextNode = JsonNode.Parse(bigJson.Value)

    let alreadyParsedSystemTextDocument = JsonDocument.Parse(bigJson.Value)

    [<Benchmark(Baseline = true)>]
    member _.AccessBigJson() =
        alreadyParsed
        |> JsonTree.getKey "definition"
        |> JsonTree.getKey "actions"
        |> JsonTree.getKey "Condition"
        |> JsonTree.getKey "actions"
        |> JsonTree.getKey "Compose"
        |> JsonTree.getKey "inputs"

    [<Benchmark>]
    member _.TryAccessBigJson() =
        alreadyParsed
        |> JsonTree.tryGetKey "definition"
        |> Option.bind (JsonTree.tryGetKey "actions")
        |> Option.bind (JsonTree.tryGetKey "Condition")
        |> Option.bind (JsonTree.tryGetKey "actions")
        |> Option.bind (JsonTree.tryGetKey "Compose")
        |> Option.bind (JsonTree.tryGetKey "inputs")
        |> Option.get

    [<Benchmark>]
    member _.TryAccessBigJsonLambdas() =
        alreadyParsed
        |> JsonTree.tryGetKey "definition"
        |> Option.bind (fun x -> JsonTree.tryGetKey "actions" x)
        |> Option.bind (fun x -> JsonTree.tryGetKey "Condition" x)
        |> Option.bind (fun x -> JsonTree.tryGetKey "actions" x)
        |> Option.bind (fun x -> JsonTree.tryGetKey "Compose" x)
        |> Option.bind (fun x -> JsonTree.tryGetKey "inputs" x)
        |> Option.get

    [<Benchmark>]
    member _.SystemTextJsonNode() =
        alreadyParsedSystemTextNode
            .AsObject()
            .["definition"].AsObject()
            .["actions"].AsObject()
            .["Condition"].AsObject()
            .["actions"].AsObject()
            .["Compose"].AsObject()
            .["inputs"]

    [<Benchmark>]
    member _.SystemTextJsonDocument() =
        alreadyParsedSystemTextDocument.RootElement
            .GetProperty("definition")
            .GetProperty("actions")
            .GetProperty("Condition")
            .GetProperty("actions")
            .GetProperty("Compose")
            .GetProperty("inputs")

type ParserBenchmark() =
    [<Benchmark(Baseline = true)>]
    member _.ParseBigJson() = JsonParser.parse bigJson.Value

    [<Benchmark>]
    member _.SystemTextJson() =
        System.Text.Json.JsonSerializer.Deserialize<System.Text.Json.Nodes.JsonNode>(bigJson.Value)

type SerialiserBenchmark() =
    let alreadyParsed = JsonParser.parse bigJson.Value

    let alreadyParsedSystemTextNode = JsonNode.Parse(bigJson.Value)

    let alreadyParsedSystemTextDocument = JsonDocument.Parse(bigJson.Value)

    [<Benchmark(Baseline = true)>]
    member _.SerialiseBigJson() =
        alreadyParsed |> Conversions.stringOfJson

    [<Benchmark>]
    member _.PrettySerialiseBigJson() =
        alreadyParsed |> Conversions.prettyStringOfJson

    [<Benchmark>]
    member _.SystemTextJsonNode() = alreadyParsedSystemTextNode.ToString()

    [<Benchmark>]
    member _.SystemTextJsonDocument() =
        alreadyParsedSystemTextDocument.RootElement.ToString()

type SchemaValidatorBenchmark() =
    let typeCheckedObjectAndArraySchema =
        """
        {
            "type": "object",
            "required": ["type", "value"],
            "oneOf": [
                {
                    "properties": {
                        "type": { "const": "array" },
                        "value": { "type": "array" }
                    }
                },
                {
                    "properties": {
                        "type": { "const": "object" },
                        "value": { "type": "object" }
                    }
                }
            ]
        }
        """

    let parsedTypeCheckedObjectAndArraySchema =
        SchemaValidator.jsonSchemaOfJson (JsonParser.parse typeCheckedObjectAndArraySchema)

    let testDataGood =
        createObject [ "type", String "object"; "value", createObject [ "1", Integer 2; "", Null ] ]

    let testDataBad =
        createObject [ "type", String "object"; "value", createArray [ Float 3.5; String "foo" ] ]

    let ``refs test schema`` =
        """
        {
            "type": "object",
            "properties": {
                "fail": { "$ref": "#/$defs/fail" },
                "recursive": { "$ref": "#" },
                "foo": { "$ref": "#/$defs/foo" }
            },
            "$defs": {
                "fail": {
                    "const": false
                },
                "foo": {
                    "type": "array",
                    "items": {
                        "$ref": "#/$defs/foo"
                    }
                }
            }
        }
        """

    let ``parsed refs test schema`` =
        SchemaValidator.jsonSchemaOfJson (JsonParser.parse ``refs test schema``)

    let testDataGoodWithRefs =
        createObject [ "foo", createArray [ emptyArray; createArray [ emptyArray ]; emptyArray ] ]

    let testDataBadWithRefs =
        createObject [ "foo", createArray [ emptyArray; createArray [ emptyObject ]; emptyArray ] ]

    [<Benchmark(Baseline = true)>]
    member _.ValidateGood() =
        SchemaValidator.validateJsonSchema parsedTypeCheckedObjectAndArraySchema testDataGood

    [<Benchmark>]
    member _.ValidateBad() =
        SchemaValidator.validateJsonSchema parsedTypeCheckedObjectAndArraySchema testDataBad

    [<Benchmark>]
    member _.ValidateGoodWithRefs() =
        SchemaValidator.validateJsonSchema ``parsed refs test schema`` testDataGoodWithRefs

    [<Benchmark>]
    member _.ValidateBadWithRefs() =
        SchemaValidator.validateJsonSchema ``parsed refs test schema`` testDataBadWithRefs

type SchemaParserBenchmark() =
    let typeCheckedObjectAndArraySchema =
        """
        {
            "type": "object",
            "required": ["type", "value"],
            "oneOf": [
                {
                    "properties": {
                        "type": { "const": "array" },
                        "value": { "type": "array" }
                    }
                },
                {
                    "properties": {
                        "type": { "const": "object" },
                        "value": { "type": "object" }
                    }
                }
            ]
        }
        """

    let halfParsedTypeCheckedObjectAndArraySchema =
        JsonParser.parse typeCheckedObjectAndArraySchema

    let ``refs test schema`` =
        """
        {
            "type": "object",
            "properties": {
                "fail": { "$ref": "#/$defs/fail" },
                "recursive": { "$ref": "#" },
                "foo": { "$ref": "#/$defs/foo" }
            },
            "$defs": {
                "fail": {
                    "const": false
                },
                "foo": {
                    "type": "array",
                    "items": {
                        "$ref": "#/$defs/foo"
                    }
                }
            }
        }
        """

    let ``half-parsed refs test schema`` = JsonParser.parse ``refs test schema``

    [<Benchmark(Baseline = true)>]
    member _.ParseSchema() =
        SchemaValidator.jsonSchemaOfJson halfParsedTypeCheckedObjectAndArraySchema

    [<Benchmark>]
    member _.ParseSchemaWithRefs() =
        SchemaValidator.jsonSchemaOfJson ``half-parsed refs test schema``
