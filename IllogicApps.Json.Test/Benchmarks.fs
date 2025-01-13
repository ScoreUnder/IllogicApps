module IllogicApps.Json.Test.Benchmarks

open System.Text.Json
open System.Text.Json.Nodes
open BenchmarkDotNet.Attributes
open IllogicApps.Json

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
    let alreadyParsed = Parser.parse bigJson.Value

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
    member _.ParseBigJson() = Parser.parse bigJson.Value

    [<Benchmark>]
    member _.SystemTextJson() =
        System.Text.Json.JsonSerializer.Deserialize<System.Text.Json.Nodes.JsonNode>(bigJson.Value)

type SerialiserBenchmark() =
    let alreadyParsed = Parser.parse bigJson.Value

    let alreadyParsedSystemTextNode = JsonNode.Parse(bigJson.Value)

    let alreadyParsedSystemTextDocument = JsonDocument.Parse(bigJson.Value)

    [<Benchmark(Baseline = true)>]
    member _.SerialiseBigJson() = alreadyParsed |> Conversions.stringOfJson

    [<Benchmark>]
    member _.PrettySerialiseBigJson() = alreadyParsed |> Conversions.prettyStringOfJson

    [<Benchmark>]
    member _.SystemTextJsonNode() = alreadyParsedSystemTextNode.ToString()

    [<Benchmark>]
    member _.SystemTextJsonDocument() = alreadyParsedSystemTextDocument.RootElement.ToString()
