// NOTICE: This file is a translation of an equivalent file in the LogicAppUnit project, and as such is licensed
// under the same terms as that project. The license can be found in this project's directory.
namespace LogicAppUnit

open System.Runtime.InteropServices

type WorkflowTestInput
    (
        workflowName: string,
        workflowDefinition: string,
        [<Optional; DefaultParameterValue(null: string)>] workflowFilename: string
    ) =
    let workflowFilename =
        if workflowFilename = null then
            "workflow.json"
        else
            workflowFilename

    member val WorkflowName = workflowName with get, set
    member val WorkflowDefinition = workflowDefinition with get, set
    member val WorkflowFilename = workflowFilename with get, set
