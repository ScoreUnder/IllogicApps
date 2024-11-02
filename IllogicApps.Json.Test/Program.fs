module IllogicApps.Json.Test.Program

open IllogicApps.Json

type Dummy() = inherit obj()

let bigJson ()=
    use stream =
        new System.IO.StreamReader(
            typeof<Dummy>.Assembly
                .GetManifestResourceStream("IllogicApps.Json.Test.TestData.SkippingTestWorkflow.json")
        )

    stream.ReadToEnd()

[<EntryPoint>]
let main args =
    let bigJson = bigJson()
    for i in 1..100000 do
        Parser.parse bigJson |> ignore
    Parser.parse bigJson |> Conversions.stringOfJson |> printfn "%s"
    0