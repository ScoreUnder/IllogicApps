module IllogicApps.Core.JsonTypeConversions

open System.Text.Json
open IllogicApps.Core.CompletedStepTypes
open IllogicApps.Json
open JsonUtil

let jsonOfCompletedTrigger (ct: CompletedTrigger) =
    let serialized = JsonSerializer.Serialize(ct, sensibleSerialiserOptions)
    JsonSerializer.Deserialize<JsonTree>(serialized, sensibleSerialiserOptions)
