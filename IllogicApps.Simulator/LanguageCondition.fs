module IllogicApps.Simulator.LanguageCondition

open System.Text.Json
open System.Text.Json.Nodes

let private toNode (a: 'a) : JsonNode =
    match box a with
    | :? JsonArray as arr -> arr
    | :? JsonObject as obj -> obj
    | _ -> JsonValue.Create<'a>(a)

let private twoArg (f: JsonNode -> JsonNode -> 'a) (args: JsonNode list) : JsonNode =
    match args with
    | [ a; b ] -> f a b |> toNode
    | _ -> failwithf "Expected 2 arguments, got %d" args.Length

let inline private ensureString (a: JsonNode) =
    match a.GetValueKind() with
    | JsonValueKind.String -> a.GetValue<string>()
    | kind -> failwithf "Expected %s to be a string, got %A" (nameof a) kind

let private compareValues f (a: JsonNode) (b: JsonNode) =
    let ka = a.GetValueKind()
    let kb = b.GetValueKind()
    if ka = kb then f (a.GetValue()) (b.GetValue())
    else failwithf "Expected both values to be of the same type, got %A and %A" ka kb

type LanguageFunction = JsonNode list -> JsonNode

let condContains =
    twoArg (fun a b ->
        match a with
        | :? JsonArray as arr -> arr.Contains(b)
        | :? JsonValue as value when value.GetValueKind() = JsonValueKind.String ->
            value.GetValue<string>().Contains(b.ToString())
        | _ -> false)

let condEquals: LanguageFunction = twoArg (=)
let condGreater: LanguageFunction = twoArg (compareValues (>))
let condGreaterOrEquals: LanguageFunction = twoArg (compareValues (>=))
let condLess: LanguageFunction = twoArg (compareValues (<))
let condLessOrEquals: LanguageFunction = twoArg (compareValues (<=))

let condStartsWith: LanguageFunction =
    twoArg (fun a b ->
        let a = ensureString a
        let b = ensureString b
        a.StartsWith(b, System.StringComparison.InvariantCulture))

let condEndsWith: LanguageFunction =
    twoArg (fun a b ->
        let a = ensureString a
        let b = ensureString b
        a.EndsWith(b, System.StringComparison.InvariantCulture))

let conditions = Map.ofList [
    "contains", condContains
    "equals", condEquals
    "greater", condGreater
    "greaterOrEquals", condGreaterOrEquals
    "less", condLess
    "lessOrEquals", condLessOrEquals
    "startsWith", condStartsWith
    "endsWith", condEndsWith
]

let (|LanguageCondition|_|) name =
    match conditions.TryGetValue(name) with
    | true, f -> Some f
    | _ -> None
