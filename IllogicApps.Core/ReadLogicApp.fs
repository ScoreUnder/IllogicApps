module IllogicApps.Core.ReadLogicApp

open System.Text.Json
open System.Text.Json.Serialization

let readLogicApp path =
    let options = new JsonSerializerOptions() in
    JsonFSharpOptions.Default()
        .WithUnionUnwrapFieldlessTags(true)
        .WithSkippableOptionFields(true)
        .WithUnionTagCaseInsensitive(true)
        .AddToJsonSerializerOptions(options)
    options.PropertyNameCaseInsensitive <- true
    options.Converters.Add(LogicAppBaseAction.ActionResolver("IllogicApps.Core.LogicAppActions"))
    let jsonText = System.IO.File.ReadAllText path in
    JsonSerializer.Deserialize<LogicAppSpec.Root>(jsonText, options)
