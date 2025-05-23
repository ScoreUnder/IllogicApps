module IllogicApps.Simulator.WorkflowFamily

open System
open IllogicApps.Core
open IllogicApps.Core.CompletedStepTypes
open IllogicApps.Core.ExternalServiceTypes
open IllogicApps.Core.LogicAppActionSupport
open IllogicApps.Json
open IllogicApps.Simulator.ExternalServices

let createAsyncBeginWorkflowResponse sim =
    { HttpRequestReply.statusCode = 202
      body = None
      headers = Some(addWorkflowResponseHeaders sim OrderedMap.empty) }

type WorkflowResponseState =
    | NotReceived
    | Ignored
    | Received

let buildWorkflowFamily
    (settingsBuilder: SimulatorCreationOptions -> ExternalServiceHandler -> SimulatorCreationOptions)
    (workflows: (string * LogicAppSpec.Root) seq)
    (name: string)
    (triggerResult: TriggerCompletion)
    =
    let workflows = workflows |> Map.ofSeq
    let rootWorkflowId = makeNewWorkflowRunId ()

    let allSims = ref []

    let rec launchWorkflow name (workflow: LogicAppSpec.Root) triggerResult responseHook runId =
        let defn = workflow.definition

        let optionsBase =
            { SimulatorCreationOptions.dummy with
                workflowName = name
                workflowId = $"id_of_{name}"
                runId = runId
                originatingRunId = rootWorkflowId
                triggerResult = triggerResult
                isStateless = Simulator.workflowIsStateless workflow }

        settingsBuilder optionsBase (handleWorkflowRequest responseHook)
        |> Simulator.Trigger (defn.triggers |> OrderedMap.toSeq |> Seq.head) defn.actions

    and handleWorkflowRequest responseHook sim =
        function
        | Workflow(workflowReq, result) ->
            let workflow = workflows |> Map.tryFind workflowReq.workflowId

            match workflow with
            | Some workflow ->
                let receivedResponse = ref NotReceived

                let nextResponseHook _innerSim resp =
                    match receivedResponse.Value with
                    | Received -> false
                    | Ignored ->
                        receivedResponse.Value <- Received
                        true
                    | NotReceived ->
                        result.Value <- resp

                        receivedResponse.Value <- Received
                        true

                if not workflowReq.asyncSupported then
                    receivedResponse.Value <- Ignored
                    result.Value <- createAsyncBeginWorkflowResponse sim

                let injectedHeaders =
                    let builder = OrderedMap.Builder().AddRange(workflowReq.headers)

                    makeWorkflowHttpRequestHeaders workflowReq.actionName sim
                    |> List.iter (fun (k, v) -> builder.Add(k, String v) |> ignore)

                    builder.Build()

                let outputs =
                    OrderedMap
                        .Builder()
                        .Add("headers", injectedHeaders |> Object)
                        .MaybeAdd("body", workflowReq.body)
                        .Build()
                    |> Object

                let newRunId = makeNewWorkflowRunId ()

                let triggerResult =
                    { CompletedAction.create
                          (workflow.definition.triggers.Keys |> Seq.head)
                          (stringOfDateTime DateTime.UtcNow) with
                        outputs = Some outputs
                        clientTrackingId = newRunId
                        originHistoryName = Some rootWorkflowId }

                // If we ever go full async, I'll need a promise for the sim here because the outer workflow might
                // finish before the inner one (because it only needs to wait until a response is received)
                let innerSim =
                    launchWorkflow
                        workflowReq.workflowId
                        workflow
                        (Completed triggerResult)
                        (Some nextResponseHook)
                        newRunId

                allSims.Value <- innerSim :: allSims.Value

                true
            | None -> false
        | HttpResponse(resp) ->
            match responseHook with
            | Some hook -> hook sim resp
            | None -> false
        | _ -> false

    let workflow = workflows |> Map.tryFind name

    match workflow with
    | Some workflow ->
        let outerSim = launchWorkflow name workflow triggerResult None rootWorkflowId
        outerSim :: allSims.Value
    | None -> failwithf "Workflow not found: %s" name
