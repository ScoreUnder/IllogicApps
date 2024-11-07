module IllogicApps.Core.LogicAppBaseAction

open CompletedStepTypes
open IllogicApps.Json

[<AbstractClass>]
type BaseAction() =
    interface IGraphExecutable with
        member this.Execute(context: SimulatorContext) = this.Execute(context)
        member this.GetChildren() = this.GetChildren()
        member this.RunAfter = this.RunAfter

    member val ActionType = "" with get, set
    member val RunAfter: OrderedMap<string, Status list> option = None with get, set
    member val TrackedProperties: OrderedMap<string, JsonTree> = OrderedMap.empty with get, set

    abstract member Execute: SimulatorContext -> ActionResult
    abstract member GetChildren: unit -> (string * IGraphExecutable) list
    default this.GetChildren() = []

    member internal this.AugmentWithJson json =
        this.ActionType <- JsonTree.getKey "type" json |> Conversions.ensureString

        this.RunAfter <-
            JsonTree.tryGetKey "runAfter" json
            |> Option.map (fun v ->
                v
                |> Conversions.ensureObject
                |> OrderedMap.mapValuesOnly (fun v ->
                    v |> Conversions.ensureArray |> Seq.map statusOfJson |> List.ofSeq))

let getAllChildren start =
    let rec aux (from: (string * IGraphExecutable) list) acc =
        from
        |> List.collect (fun (_, a) -> a.GetChildren())
        |> function
            | [] -> from @ acc
            | next -> aux next (from @ acc) in

    aux start []

type UnknownAction() =
    inherit BaseAction()
    member val Original = Conversions.emptyObject with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Unknown action: %A" this.Original

        { status = Skipped
          inputs = None
          outputs = None }

    static member OfJson json =
        let r = UnknownAction()
        r.Original <- json
        r.AugmentWithJson json
        r
