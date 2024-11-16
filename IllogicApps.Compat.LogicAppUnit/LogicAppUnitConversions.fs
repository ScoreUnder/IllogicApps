module IllogicApps.Compat.LogicAppUnit.LogicAppUnitConversions

open IllogicApps.Core.CompletedStepTypes
open LogicAppUnit

let actionStatusOfIllogic =
    function
    | Status.Succeeded -> ActionStatus.Succeeded
    | Status.Failed -> ActionStatus.Failed
    | Status.Skipped -> ActionStatus.Skipped
    | Status.TimedOut -> ActionStatus.TimedOut
    | Status.Cancelled -> ActionStatus.Cancelled

let workflowRunStatusOfIllogic =
    function
    | Status.Succeeded -> WorkflowRunStatus.Succeeded
    | Status.Failed -> WorkflowRunStatus.Failed
    | Status.Skipped -> WorkflowRunStatus.NotTriggered
    | Status.TimedOut -> WorkflowRunStatus.TimedOut
    | Status.Cancelled -> WorkflowRunStatus.Cancelled
