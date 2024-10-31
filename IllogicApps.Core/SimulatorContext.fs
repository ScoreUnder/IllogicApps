namespace IllogicApps.Core

open System
open System.Collections.Generic
open System.Text.Json.Nodes

open CompletedStepTypes
open ExternalServiceTypes

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
    abstract member Execute: SimulatorContext -> ActionResult
    abstract member RunAfter: Map<string, Status list> option with get
    abstract member GetChildren: unit -> (string * IGraphExecutable) list

and [<AbstractClass>] SimulatorContext() =
    /// All variables active in the current workflow.
    member val Variables = Dictionary<string, JsonNode>() with get

    /// Indicates if the simulator is bug-for-bug accurate.
    /// e.g. if large integers should lose precision when parsed with int('...')
    abstract member IsBugForBugAccurate: bool

    /// Gets the current loop context.
    abstract member LoopContext: LoopContext

    /// Gets the current array operation context.
    abstract member ArrayOperationContext: LoopContext

    /// Gets the result of the trigger which invoked this workflow.
    abstract member GetTriggerResult: CompletedTrigger

    /// Gets the result of an action by its name.
    abstract member GetActionResult: string -> CompletedAction option

    /// Executes a graph of actions.
    abstract member ExecuteGraph: Map<string, #IGraphExecutable> -> Status

    /// Stops the execution with a given status. Used by the Terminate action.
    abstract member StopExecuting: Status -> unit

    /// Evaluates a condition expression.
    /// Example: EvaluateCondition {"and": [{"equals": [1, 2]}]}
    /// Result: false
    abstract member EvaluateCondition: Expression -> bool

    /// Evaluates a language expression.
    /// Example: EvaluateLanguage {"@concat('hello', ' world')": "@add(1, 2)"}
    /// Result: {"hello world": 3}
    abstract member EvaluateLanguage: JsonNode -> JsonNode

    /// Sends an external service request.
    /// This refers to anything outside the current workflow:
    /// Other workflow invocations, HTTP requests, filesystem access, etc.
    abstract member ExternalServiceRequest: ExternalServiceRequestType -> unit

    /// Pushes a new loop context (i.e. sets a new default context for the items() expression).
    /// Remember to call Dispose() on the returned context when done.
    abstract member PushLoopContext: JsonNode seq -> LoopContext

    /// Pushes a new array operation context (i.e. sets a new context for the item() expression)
    abstract member PushArrayOperationContext: JsonNode seq -> LoopContext

    /// Mark all provided actions, and their children, as skipped
    abstract member ForceSkipAll: (string * #IGraphExecutable) seq -> unit
