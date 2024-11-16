// NOTICE: This file is a translation of an equivalent file in the LogicAppUnit project, and as such is licensed
// under the same terms as that project. The license can be found in this project's directory.
namespace LogicAppUnit

type ActionStatus =
    | Aborted = 0
    | Cancelled = 1
    | Failed = 2
    | Running = 3
    | Skipped = 4
    | Succeeded = 5
    | TimedOut = 6
    | Waiting = 7
