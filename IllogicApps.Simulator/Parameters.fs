module IllogicApps.Simulator.Parameters

open IllogicApps.Core.LogicAppActionSupport
open IllogicApps.Json

let parameterTypeOfString (s: string) =
    match s.ToLowerInvariant() with
    | "string" -> VariableType.String
    | "int" -> VariableType.Integer
    | "float" -> VariableType.Float
    | "boolean" -> VariableType.Boolean
    | "object" -> VariableType.Object
    | "array" -> VariableType.Array
    | _ -> failwithf "Unknown variable type: %s" s

let parameterTypeOfJson (json: JsonTree) =
    json |> Conversions.ensureString |> parameterTypeOfString

type Parameter =
    { type_: VariableType; value: JsonTree }

let parameterOfJson json =
    { type_ = JsonTree.getKeyCaseInsensitive "type" json |> parameterTypeOfJson
      value = JsonTree.getKeyCaseInsensitive "value" json }

let assertParameterType (param: Parameter) =
    if param.value = Null then
        failwith "Parameter value is null"

    let actual = getVarType param.value
    let expected = param.type_

    if actual <> expected then
        failwithf "Parameter's value has type %O, expected %O" actual expected

    param
