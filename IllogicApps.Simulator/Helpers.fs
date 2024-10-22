module private IllogicApps.Simulator.Helpers

open System.Text.Json
open System.Text.Json.Nodes

let inline ensureString (a: JsonNode) =
    match a.GetValueKind() with
    | JsonValueKind.String -> a.GetValue<string>()
    | kind -> failwithf "Expected %s to be a string, got %A" (nameof a) kind

let inline optionToNull (a: JsonNode option) =
    match a with
    | Some v -> v
    | None -> JsonValue.Create(JsonValueKind.Null)
