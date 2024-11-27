namespace IllogicApps.Core

open System

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

type ActionRepetition =
    { loopName: string
      repetitionCount: int }

type ArrayOperationContext =
    inherit IDisposable
    abstract Advance: unit -> bool
    abstract Current: JsonTree


[<AbstractClass>]
type BaseAction(json: JsonTree) =
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

    abstract Execute: string -> SimulatorContext -> ActionResult
    abstract GetChildren: unit -> (string * BaseAction) seq
    default this.GetChildren() = []

    static member GetChildren(a: BaseAction) = a.GetChildren() |> List.ofSeq

and ActionGraph = OrderedMap<string, BaseAction>

and ScopeContext =
    inherit IDisposable
    abstract Run: ActionGraph -> Status
    abstract MergeResult: Status -> unit
    abstract OverallResult: Status

and SimulatorContext =
    /// Get a variable from the current execution context.
    abstract GetVariable: string -> JsonTree option

    /// Set a variable in the current execution context.
    abstract SetVariable: string -> JsonTree -> unit

    /// Get a value from App Config
    abstract GetAppConfig: string -> string option

    /// Get a value from parameters
    abstract GetParameter: string -> JsonTree option

    /// Indicates if the simulator is bug-for-bug accurate.
    /// e.g. if large integers should lose precision when parsed with int('...')
    abstract IsBugForBugAccurate: bool

    /// Gets the current array operation context.
    abstract ArrayOperationContext: ArrayOperationContext option

    /// Gets the result of the trigger which invoked this workflow.
    abstract TriggerResult: CompletedAction

    /// Gets the results of all actions executed so far.
    abstract AllActionResults: OrderedMap<string, CompletedAction>

    /// Gets the details of the workflow. Mostly crap & made for compatibility but I bet nobody will care lol
    abstract WorkflowDetails: WorkflowDetails

    /// Gets the result of an action by its name.
    abstract GetActionResult: string -> CompletedAction option

    /// Gets the results from each repetition of an action in a loop.
    abstract GetActionRepetitions: string -> (ActionRepetition list * CompletedAction) list

    /// Stops the execution with a given status. Used by the Terminate action.
    abstract Terminate: Status -> TerminateRunError option -> unit

    /// Evaluates a condition expression.
    /// Example: EvaluateCondition {"and": [{"equals": [1, 2]}]}
    /// Result: false
    abstract EvaluateCondition: Expression -> bool

    /// Evaluates a language expression.
    /// Example: EvaluateLanguage {"@concat('hello', ' world')": "@add(1, 2)"}
    /// Result: {"hello world": 3}
    abstract EvaluateLanguage: JsonTree -> JsonTree

    /// Sends an external service request.
    /// This refers to anything outside the current workflow:
    /// Other workflow invocations, HTTP requests, filesystem access, etc.
    abstract ExternalServiceRequest: ExternalServiceRequest -> unit

    /// Pushes a new array operation context (i.e. sets a new context for the item()/items() expression)
    /// Remember to call Dispose() on the returned context when done.
    abstract PushArrayOperationContext: scopeName: string option -> arrayItems: JsonTree seq -> ArrayOperationContext

    /// Gets a current array operation context by name.
    abstract GetArrayOperationContextByName: string -> ArrayOperationContext option

    /// Pushes a new scope context.
    /// Remember to call Dispose() on the returned context when done.
    abstract PushScopeContext:
        scopeName: string -> isRepeating: bool -> potentialActions: (string * BaseAction) seq -> ScopeContext

[<AbstractClass>]
type BaseTrigger(json) =
    inherit BaseAction(json)

    member val ClientTrackingId =
        JsonTree.tryGetKey "correlation" json
        |> Option.bind (JsonTree.tryGetKey "clientTrackingId") with get

    abstract member ProcessInputs: SimulatorContext -> JsonTree option

    override this.ProcessInputs _context = None

    abstract member RunFromRequest: HttpRequest -> SimulatorContext -> ActionResult

    override this.Execute (_: string) (context: SimulatorContext) =
        // TODO: Does this ever get called?
        // On a well-formed workflow that is. I think there is a way to add triggers to the
        // action graph via the designer but I don't know how that translates to actual
        // behaviour, and I have not yet tried it because I don't know if anyone would do it
        // for any reason other than curiosity.
        let triggerResult = context.TriggerResult

        { status = triggerResult.status
          inputs = triggerResult.inputs
          outputs = triggerResult.outputs
          code = triggerResult.code
          error = triggerResult.error }

module BaseAction =
    let getAllChildren start =
        let rec aux from acc =
            from
            |> List.collect (fun (_, a: BaseAction) -> a.GetChildren() |> List.ofSeq)
            |> function
                | [] -> from @ acc
                | next -> aux next (from @ acc) in

        aux start []
