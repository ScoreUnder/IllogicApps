module IllogicApps.Simulator.Parameters

open IllogicApps.Core
open IllogicApps.Core.LogicAppActionSupport
open IllogicApps.Json

type Parameter =
    { type_: VariableType; value: JsonTree }

let parameterOfJson json =
    { type_ = JsonTree.getKey "type" json |> variableTypeOfJson
      value = JsonTree.getKey "value" json }

let evaluateParameter (simContext: SimulatorContext) (param: Parameter) : JsonTree =
    simContext.EvaluateLanguage param.value |> coerce param.type_
