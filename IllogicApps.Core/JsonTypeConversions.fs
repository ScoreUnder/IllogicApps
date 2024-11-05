module IllogicApps.Core.JsonTypeConversions

open System.Text.Json
open IllogicApps.Core.CompletedStepTypes
open IllogicApps.Json
open JsonUtil

let jsonOfCompletedTrigger (ct: CompletedTrigger) =
    let options = sensibleSerialiserOptions ()
    let serialized = JsonSerializer.Serialize(ct, options)
    JsonSerializer.Deserialize<JsonTree>(serialized, options)
