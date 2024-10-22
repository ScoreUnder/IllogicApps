module IllogicApps.Simulator.LanguageEvaluator

open System.Text.Json.Nodes

let evaluateIfNecessary simContext (rawStr: string) : JsonNode =
    if LanguageLexer.isLiteralStringWithAtSign rawStr then
        JsonValue.Create(rawStr.[1..])
    else if LanguageLexer.requiresInterpolation rawStr then
        let lexed = LanguageLexer.lex rawStr
        let parsed = LanguageParser.parse lexed
        printfn "Not implemented: expression evaluation: %A\n%A\n%A" rawStr lexed parsed
        JsonValue.Create(rawStr)
    else
        JsonValue.Create(rawStr)
