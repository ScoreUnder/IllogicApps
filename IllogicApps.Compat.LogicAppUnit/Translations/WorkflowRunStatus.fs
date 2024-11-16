// NOTICE: This file is a translation of an equivalent file in the LogicAppUnit project, and as such is licensed
// under the same terms as that project. The license can be found in this project's directory.
namespace LogicAppUnit

type WorkflowRunStatus =
    | NotTriggered = 0
    | Aborted = 1
    | Cancelled = 2
    | Failed = 3
    | Running = 4
    | Succeeded = 5
    | TimedOut = 6
    | Waiting = 7
