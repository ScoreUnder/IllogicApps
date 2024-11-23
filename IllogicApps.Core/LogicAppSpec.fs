module IllogicApps.Core.LogicAppSpec

open IllogicApps.Json

let actionGraphOfJson resolveAction json =
    json |> Conversions.ensureObject |> OrderedMap.mapValuesOnly resolveAction

type Definition =
    { actions: ActionGraph
      outputs: OrderedMap<string, JsonTree>
      triggers: ActionGraph }

let definitionOfJson resolveAction json =
    { actions = JsonTree.getKey "actions" json |> actionGraphOfJson resolveAction
      outputs = JsonTree.getKeyMapOrElse "outputs" Conversions.ensureObject (fun () -> OrderedMap.empty) json
      triggers = JsonTree.getKey "triggers" json |> actionGraphOfJson resolveAction }

type Root =
    { definition: Definition; kind: string }

let rootOfJson resolveAction json =
    { definition = JsonTree.getKey "definition" json |> definitionOfJson resolveAction
      kind = JsonTree.getKey "kind" json |> Conversions.ensureString }
