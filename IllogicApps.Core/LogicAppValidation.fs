module IllogicApps.Core.LogicAppValidation

open IllogicApps.Core.LogicAppBaseAction
open IllogicApps.Core.LogicAppSpec
open IllogicApps.Json

let containsResponseAction (actionGraph: ActionGraph) =
    actionGraph
    |> OrderedMap.toList
    |> getAllChildrenF BaseAction.GetChildren
    |> List.exists (fun (_, a) -> a.ActionType = "Response")
