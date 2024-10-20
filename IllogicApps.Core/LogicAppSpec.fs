namespace IllogicApps.Core.LogicAppSpec

open System.Collections.Generic
open System.Text.Json.Nodes

open IllogicApps.Core.LogicAppBaseAction

[<AbstractClass>]
type Action() =
    inherit BaseAction()
    member val TrackedProperties: IDictionary<string, JsonNode option> option = None with get, set

type ActionGraph = Map<string, BaseAction>

type Definition =
    { actions: ActionGraph
      outputs: Map<string, JsonObject>
      triggers: ActionGraph }

type Root =
    { definition: Definition; kind: string }
