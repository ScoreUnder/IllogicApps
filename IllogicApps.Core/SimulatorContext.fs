namespace IllogicApps.Core

open System
open System.Collections.Generic
open System.Text.Json.Nodes
open ExternalServiceTypes

type Status =
    | Succeeded
    | Failed
    | Skipped
    | TimedOut

type Expression = JsonObject

type ActionResult =
    { status: Status
      inputs: JsonNode option
      outputs: JsonNode option }

[<AbstractClass>]
type LoopContext() =
    interface IDisposable with
        member this.Dispose() : unit = this.Dispose()

    abstract member Dispose: unit -> unit
    abstract member Advance: unit -> bool
    abstract member Current: JsonNode

type IGraphExecutable =
    abstract Execute: SimulatorContext -> ActionResult
    abstract member RunAfter: Map<string, Status list> option with get
    abstract member GetChildren: unit -> (string * IGraphExecutable) list

and [<AbstractClass>] SimulatorContext(triggerOutput: JsonNode) =
    member val Variables = new Dictionary<string, JsonNode>() with get
    member val TriggerOutput = triggerOutput
    abstract member ExecuteGraph: Map<string, #IGraphExecutable> -> Status
    abstract member StopExecuting: Status -> unit
    abstract member EvaluateCondition: Expression -> bool
    abstract member EvaluateLanguage: JsonNode -> JsonNode
    abstract member ExternalServiceRequest: ExternalServiceRequestType -> unit
    abstract member PushLoopContext: JsonNode seq -> LoopContext
    abstract member ForceSkipAll: (string * #IGraphExecutable) seq -> unit
