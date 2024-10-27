module IllogicApps.Core.LogicAppBaseAction

open System.Text.Json.Nodes
open System.Text.Json.Serialization

open CompletedStepTypes

[<AbstractClass>]
type BaseAction() =
    interface IGraphExecutable with
        member this.Execute(context: SimulatorContext) = this.Execute(context)
        member this.GetChildren() = this.GetChildren()
        member this.RunAfter = this.RunAfter

    [<JsonPropertyName("type")>]
    member val ActionType = "" with get, set

    member val RunAfter: Map<string, Status list> option = None with get, set

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

type UnknownAction() =
    inherit BaseAction()
    member val Original = JsonObject() with get, set

    override this.Execute(context: SimulatorContext) =
        printfn "Unknown action: %A" this.Original

        { status = Skipped
          inputs = None
          outputs = None }
