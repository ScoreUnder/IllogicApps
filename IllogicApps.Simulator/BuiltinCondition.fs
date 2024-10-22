module IllogicApps.Simulator.BuiltinCondition

open System
open System.Text.Json
open System.Text.Json.Nodes
open Helpers

let private toNode (a: 'a) : JsonNode =
    match box a with
    | :? JsonArray as arr -> arr
    | :? JsonObject as obj -> obj
    | _ -> JsonValue.Create<'a>(a)

let private twoArg (f: JsonNode -> JsonNode -> 'a) (args: JsonNode list) : JsonNode =
    match args with
    | [ a; b ] -> f a b |> toNode
    | _ -> failwithf "Expected 2 arguments, got %d" args.Length

type private ComparisonResult =
    | Equal
    | Greater
    | Less
    | Incomparable

let private compareStrings a b =
    match String.Compare(a, b, StringComparison.InvariantCulture) with
    | 0 -> Equal
    | n when n > 0 -> Greater
    | _ -> Less

let private compareNumbers a b =
    if Double.IsNaN(a) || Double.IsNaN(b) then Incomparable
    elif a = b then Equal
    elif a > b then Greater
    else Less

let private compareValues (a: JsonNode) (b: JsonNode) : ComparisonResult =
    let ka = a.GetValueKind()
    let kb = b.GetValueKind()

    if ka = kb then
        match ka with
        | JsonValueKind.String -> compareStrings (a.GetValue<string>()) (b.GetValue<string>())
        | JsonValueKind.Number -> compareNumbers (a.GetValue<float>()) (b.GetValue<float>())
        | JsonValueKind.Array -> Incomparable // TODO is this how it works?
        | JsonValueKind.Object -> Incomparable
        | JsonValueKind.Null
        | JsonValueKind.False
        | JsonValueKind.True -> Equal
        | _ -> failwith "Unknown enum value"
    else
        match ka, kb with
        | JsonValueKind.String, JsonValueKind.Number -> compareStrings (a.ToString()) (b.ToString()) // TODO does this really coerce?
        | JsonValueKind.Number, JsonValueKind.String -> compareStrings (a.ToString()) (b.ToString())
        | _ -> Incomparable

let private checkComparison results a b =
    List.contains (compareValues a b) results

type LanguageFunction = JsonNode list -> JsonNode

let condContains =
    twoArg (fun a b ->
        match a with
        | :? JsonArray as arr -> arr.Contains(b)
        | :? JsonValue as value when value.GetValueKind() = JsonValueKind.String ->
            value.GetValue<string>().Contains(b.ToString())
        | _ -> false)

let condEquals: LanguageFunction = twoArg (checkComparison [ Equal ])
let condGreater: LanguageFunction = twoArg (checkComparison [ Greater ])

let condGreaterOrEquals: LanguageFunction =
    twoArg (checkComparison [ Greater; Equal ])

let condLess: LanguageFunction = twoArg (checkComparison [ Less ])
let condLessOrEquals: LanguageFunction = twoArg (checkComparison [ Less; Equal ])

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

type LanguageCondition = JsonNode list -> JsonNode

let conditions: Map<string, LanguageCondition> =
    Map.ofList
        [ "contains", condContains
          "equals", condEquals
          "greater", condGreater
          "greaterOrEquals", condGreaterOrEquals
          "less", condLess
          "lessOrEquals", condLessOrEquals
          "startsWith", condStartsWith
          "endsWith", condEndsWith ]

let (|LanguageCondition|_|) name =
    match conditions.TryGetValue(name) with
    | true, f -> Some f
    | _ -> None
