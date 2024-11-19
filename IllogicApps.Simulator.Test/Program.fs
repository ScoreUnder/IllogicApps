module IllogicApps.Json.Test.Program

open System.Reflection
open BenchmarkDotNet.Reports
open BenchmarkDotNet.Running

[<EntryPoint>]
let main args =
    let anyErrors =
        BenchmarkSwitcher.FromAssembly(Assembly.GetCallingAssembly()).Run(args)
        |> Seq.exists (fun (result: Summary) -> result.HasCriticalValidationErrors)

    if anyErrors then 1 else 0
