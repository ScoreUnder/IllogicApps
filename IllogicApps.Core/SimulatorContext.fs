namespace IllogicApps.Core

open System.Collections.Generic
open System.Text.Json.Nodes

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

type IGraphExecutable =
    abstract Execute: SimulatorContext -> ActionResult
    abstract member RunAfter: Map<string, Status list> option with get

and [<AbstractClass>] SimulatorContext(triggerOutput: JsonNode) =
    member val Variables = new Dictionary<string, JsonNode>() with get
    member val TriggerOutput = triggerOutput
    abstract member ExecuteGraph: Map<string, #IGraphExecutable> -> Status
    abstract member StopExecuting: Status -> unit
    abstract member EvaluateCondition: Expression -> bool
    abstract member EvaluateLanguage: JsonNode -> JsonNode
