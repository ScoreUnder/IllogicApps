module IllogicApps.Simulator.BuiltinFunctions

open System.Text.Json
open System.Text.Json.Nodes
open IllogicApps.Core
open Helpers

type LanguageFunction = SimulatorContext -> JsonNode list -> JsonNode
type Args = JsonNode list

let expectArgs n (args: Args) =
    if args.Length <> n then
        failwithf "Expected %d arguments, got %d" n args.Length

let expectArgsRange min max (args: Args) =
    if args.Length < min || args.Length > max then
        failwithf "Expected between %d and %d arguments, got %d" min max args.Length

let expectArgsAtLeast n (args: Args) =
    if args.Length < n then
        failwithf "Expected at least %d arguments, got %d" n args.Length

let toBase64 (str: string) =
    str |> System.Text.Encoding.UTF8.GetBytes |> System.Convert.ToBase64String

let fromBase64 (str: string) =
    str |> System.Convert.FromBase64String |> System.Text.Encoding.UTF8.GetString

let objectToString (node: JsonNode) : string =
    match node.GetValueKind() with
    | JsonValueKind.Object ->
        let node = node.AsObject()

        if node.ContainsKey("$content-type") && node.ContainsKey("$content") then
            let content = node["$content"].ToString()
            content |> fromBase64
        else
            node.ToString()
    | _ -> node.ToString()

// String functions

let f_concat _ (args: Args) : JsonNode =
    expectArgsAtLeast 1 args

    args
    |> List.map objectToString
    |> String.concat ""
    |> fun v -> JsonValue.Create(v)

let f_startsWith _ (args: Args) : JsonNode =
    match args with
    | [ a; b ] ->
        match a.GetValueKind(), b.GetValueKind() with
        | JsonValueKind.String, JsonValueKind.String ->
            let a = a.GetValue<string>()
            let b = b.GetValue<string>()
            JsonValue.Create(a.StartsWith(b))
        | kindA, kindB -> failwithf "Expected strings, got %A and %A" kindA kindB
    | _ -> failwith "Expected 2 arguments"

// Collection functions

let f_item (sim: SimulatorContext) (args: Args) : JsonNode =
    expectArgs 0 args

    sim.ArrayOperationContext.Current.DeepClone()

// Logical comparison funtions

let f_not _ (args: Args) : JsonNode =
    expectArgs 1 args
    let value = List.head args

    match value.GetValueKind() with
    | JsonValueKind.False -> JsonValue.Create(true)
    | JsonValueKind.True -> JsonValue.Create(false)
    | kind -> failwithf "Expected boolean, got %A" kind

// Conversion functions

let f_string _ (args: Args) : JsonNode =
    expectArgs 1 args
    JsonValue.Create(args |> List.head |> objectToString)

// Workflow functions

let f_outputs (sim: SimulatorContext) (args: Args) : JsonNode =
    expectArgs 1 args
    let actionName = ensureString <| List.head args

    match sim.GetActionResult actionName with
    | Some result -> result.outputs |> Option.map _.DeepClone() |> optionToNull
    | None -> failwithf "Action %s not found" actionName

let f_trigger (sim: SimulatorContext) (args: Args) : JsonNode =
    expectArgs 0 args

    // TODO: Implement trigger() function
    new JsonObject()

let f_variables (sim: SimulatorContext) (args: Args) : JsonNode =
    expectArgs 1 args
    let variableName = ensureString <| List.head args

    match sim.Variables.TryGetValue variableName with
    | true, value -> value
    | _ -> failwithf "Variable %s not found" variableName

// End function definitions

let private conditionToFunction (condition: BuiltinCondition.LanguageCondition) : LanguageFunction = fun _ -> condition

let functions: Map<string, LanguageFunction> =
    let conditions =
        BuiltinCondition.conditions
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> k, conditionToFunction v)

    [ "concat", f_concat
      "startsWith", f_startsWith
      "item", f_item
      "not", f_not
      "string", f_string
      "outputs", f_outputs
      "trigger", f_trigger
      "variables", f_variables ]
    |> List.toSeq
    |> Seq.append conditions
    |> Map.ofSeq
