module IllogicApps.Core.LogicAppValidation

open IllogicApps.Json

let containsResponseAction (actionGraph: ActionGraph) =
    actionGraph
    |> OrderedMap.toList
    |> BaseAction.getAllChildren
    |> List.exists (fun (_, a) -> a.ActionType = "Response")
