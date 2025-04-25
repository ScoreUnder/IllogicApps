module IllogicApps.Simulator.ReadSupportingData

open IllogicApps.Json
open Parameters

let appSettingsOfJson json =
    json
    |> JsonTree.getKey "Values"
    |> Conversions.ensureObject
    |> OrderedMap.map (fun k v -> k.Replace("__", ":"), Conversions.ensureString v)

let readAppSettings path =
    path |> System.IO.File.ReadAllText |> JsonParser.parse |> appSettingsOfJson

let parametersOfJson json =
    json
    |> Conversions.ensureObject
    |> OrderedMap.mapValuesOnly (fun v -> assertParameterType (parameterOfJson v))

let readParameters path =
    path |> System.IO.File.ReadAllText |> JsonParser.parse |> parametersOfJson
