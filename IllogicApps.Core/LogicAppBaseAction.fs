module IllogicApps.Core.LogicAppBaseAction

open CompletedStepTypes
open IllogicApps.Json

[<AbstractClass>]
type BaseAction(json: JsonTree) =
    interface IGraphExecutable with
        member this.Execute (name: string) (context: SimulatorContext) = this.Execute name context

        member this.GetChildren() =
            this.GetChildren()
            |> Seq.map (fun (n, a) -> n, (a: IGraphExecutable))
            |> List.ofSeq

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

    abstract member Execute: string -> SimulatorContext -> ActionResult
    abstract member GetChildren: unit -> (string * BaseAction) seq
    default this.GetChildren() = []

    static member GetChildren(a: BaseAction) = a.GetChildren() |> List.ofSeq

let getAllChildrenF fc start =
    let rec aux from acc =
        from
        |> List.collect (fun (_, a) -> fc a)
        |> function
            | [] -> from @ acc
            | next -> aux next (from @ acc) in

    aux start []

let getAllChildren = getAllChildrenF (fun (a: IGraphExecutable) -> a.GetChildren())

type UnknownAction(json) =
    inherit BaseAction(json)
    member val Original = json with get

    override this.Execute (name: string) (context: SimulatorContext) =
        printfn "Unknown action: %s" <| Conversions.prettyStringOfJson this.Original

        { ActionResult.Default with
            status = Skipped }
