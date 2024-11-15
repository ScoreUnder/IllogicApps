namespace IllogicApps.Core

open System
open System.Collections.Generic

open CompletedStepTypes
open ExternalServiceTypes
open IllogicApps.Json

type Expression = JsonTree

type ActionResult =
    { status: Status
      code: ActionCode option
      error: ActionError option
      inputs: JsonTree option
      outputs: JsonTree option }

    static member Default =
        { status = Succeeded
          code = None
          error = None
          inputs = None
          outputs = None }

[<AbstractClass>]
type LoopContext() =
    interface IDisposable with
        member this.Dispose() : unit = this.Dispose()

    abstract member Dispose: unit -> unit
    abstract member Advance: unit -> bool
    abstract member Current: JsonTree

type IGraphExecutable =
    abstract member Execute: SimulatorContext -> ActionResult
    abstract member RunAfter: OrderedMap<string, Status list> option with get
    abstract member GetChildren: unit -> (string * IGraphExecutable) list

and [<AbstractClass>] SimulatorContext() =
    /// Get a variable from the current execution context.
    abstract member GetVariable: string -> JsonTree option

    /// Set a variable in the current execution context.
    abstract member SetVariable: string -> JsonTree -> unit

    /// Get a value from App Config
    abstract member GetAppConfig: string -> string option

    /// Get a value from parameters
    abstract member GetParameter: string -> JsonTree option

    /// Indicates if the simulator is bug-for-bug accurate.
    /// e.g. if large integers should lose precision when parsed with int('...')
    abstract member IsBugForBugAccurate: bool

    /// Gets the current loop context.
    abstract member LoopContext: LoopContext

    /// Gets the current array operation context.
    abstract member ArrayOperationContext: LoopContext

    /// Gets the result of the trigger which invoked this workflow.
    abstract member TriggerResult: CompletedTrigger

    /// Gets the results of all actions executed so far.
    abstract member AllActionResults: OrderedMap<string, CompletedAction>

    /// Gets the details of the workflow. Mostly crap & made for compatibility but I bet nobody will care lol
    abstract member WorkflowDetails: WorkflowDetails

    /// Gets the result of an action by its name.
    abstract member GetActionResult: string -> CompletedAction option

    /// Executes a graph of actions.
    abstract member ExecuteGraph: OrderedMap<string, #IGraphExecutable> -> Status

    /// Stops the execution with a given status. Used by the Terminate action.
    abstract member StopExecuting: Status -> unit

    /// Evaluates a condition expression.
    /// Example: EvaluateCondition {"and": [{"equals": [1, 2]}]}
    /// Result: false
    abstract member EvaluateCondition: Expression -> bool

    /// Evaluates a language expression.
    /// Example: EvaluateLanguage {"@concat('hello', ' world')": "@add(1, 2)"}
    /// Result: {"hello world": 3}
    abstract member EvaluateLanguage: JsonTree -> JsonTree

    /// Sends an external service request.
    /// This refers to anything outside the current workflow:
    /// Other workflow invocations, HTTP requests, filesystem access, etc.
    abstract member ExternalServiceRequest: ExternalServiceRequest -> unit

    /// Pushes a new loop context (i.e. sets a new default context for the items() expression).
    /// Remember to call Dispose() on the returned context when done.
    abstract member PushLoopContext: JsonTree seq -> LoopContext

    /// Pushes a new array operation context (i.e. sets a new context for the item() expression)
    abstract member PushArrayOperationContext: JsonTree seq -> LoopContext

    /// Mark all provided actions, and their children, as skipped
    abstract member ForceSkipAll: (string * #IGraphExecutable) seq -> unit
