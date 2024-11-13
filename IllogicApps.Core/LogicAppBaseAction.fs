module IllogicApps.Core.LogicAppBaseAction

open CompletedStepTypes
open IllogicApps.Json

[<AbstractClass>]
type BaseAction(json: JsonTree) =
    interface IGraphExecutable with
        member this.Execute(context: SimulatorContext) = this.Execute(context)
        member this.GetChildren() = this.GetChildren()
        member this.RunAfter = this.RunAfter

    member val ActionType = JsonTree.getKey "type" json |> Conversions.ensureString with get

    member val RunAfter =
        JsonTree.tryGetKey "runAfter" json
        |> Option.map (fun v ->
            v
            |> Conversions.ensureObject
            |> OrderedMap.mapValuesOnly (fun v -> v |> Conversions.ensureArray |> Seq.map statusOfJson |> List.ofSeq)) with get

    member val TrackedProperties: OrderedMap<string, JsonTree> =
        JsonTree.tryGetKey "trackedProperties" json
        |> Option.map Conversions.ensureObject
        |> Option.defaultValue OrderedMap.empty with get

    abstract member Execute: SimulatorContext -> ActionResult
    abstract member GetChildren: unit -> (string * IGraphExecutable) list
    default this.GetChildren() = []


let getAllChildren start =
    let rec aux (from: (string * IGraphExecutable) list) acc =
        from
        |> List.collect (fun (_, a) -> a.GetChildren())
        |> function
            | [] -> from @ acc
            | next -> aux next (from @ acc) in

    aux start []

type UnknownAction(json) =
    inherit BaseAction(json)
    member val Original = json with get

    override this.Execute(context: SimulatorContext) =
        printfn "Unknown action: %s" <| Conversions.prettyStringOfJson this.Original

        { ActionResult.Default with
            status = Skipped }
