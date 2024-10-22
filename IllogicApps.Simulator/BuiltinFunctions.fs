module IllogicApps.Simulator.BuiltinFunctions

open System.Text.Json
open System.Text.Json.Nodes
open IllogicApps.Core
open Helpers
open System.Collections.Generic

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

let toBinary (str: string) : JsonNode =
    let base64 = str |> toBase64 in

    new JsonObject(
        [ new KeyValuePair<string, JsonNode>("$content-type", JsonValue.Create("application/octet-stream"))
          new KeyValuePair<string, JsonNode>("$content", JsonValue.Create(base64)) ]
    )

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

let f_base64 _ (args: Args) : JsonNode =
    expectArgs 1 args
    let str = ensureString <| List.head args

    JsonValue.Create(toBase64 str)

let f_base64ToBinary _ (args: Args) : JsonNode =
    expectArgs 1 args
    let str = ensureString <| List.head args

    str |> fromBase64 |> toBinary

let f_base64ToString _ (args: Args) : JsonNode =
    expectArgs 1 args
    let str = ensureString <| List.head args

    str |> fromBase64 |> (fun v -> JsonValue.Create(v))

let f_binary _ (args: Args) : JsonNode =
    expectArgs 1 args
    let str = ensureString <| List.head args

    toBinary str

let f_decimal _ (args: Args) : JsonNode =
    expectArgs 1 args
    let str = objectToString <| List.head args

    // TODO: verify how it parses
    // TODO: do arithmetic on decimals?
    match
        System.Decimal.TryParse(
            str,
            System.Globalization.NumberStyles.Any,
            System.Globalization.CultureInfo.InvariantCulture
        )
    with
    | true, result -> JsonValue.Create(result)
    | _ -> failwithf "Could not parse %s as decimal" str

let f_json _ (args: Args) : JsonNode =
    expectArgs 1 args
    let str = ensureString <| List.head args

    JsonNode.Parse(str)

let f_string _ (args: Args) : JsonNode =
    expectArgs 1 args
    JsonValue.Create(args |> List.head |> objectToString)

// Math functions

let f_sub _ (args: Args) : JsonNode =
    match args with
    | [ a; b ] ->
        match a.GetValueKind(), b.GetValueKind() with
        | JsonValueKind.Number, JsonValueKind.Number ->
            let a = box <| a.GetValue()
            let b = box <| b.GetValue()

            match a, b with
            | :? int as a, (:? int as b) -> JsonValue.Create(a - b)
            | :? float as a, (:? float as b) -> JsonValue.Create(a - b)
            | :? int as a, (:? float as b) -> JsonValue.Create(float a - b)
            | :? float as a, (:? int as b) -> JsonValue.Create(a - float b)
            | :? decimal as a, (:? decimal as b) -> JsonValue.Create(a - b)
            | :? decimal as a, (:? int as b) -> JsonValue.Create(a - decimal b)
            | :? int as a, (:? decimal as b) -> JsonValue.Create(decimal a - b)
            | :? float as a, (:? decimal as b) -> JsonValue.Create(decimal a - b)
            | :? decimal as a, (:? float as b) -> JsonValue.Create(a - decimal b)
            | _ -> failwithf "Expected numbers, got %A and %A" (a.GetType()) (b.GetType())
        | kindA, kindB -> failwithf "Expected numbers, got %A and %A" kindA kindB
    | _ -> failwith "Expected 2 arguments"

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
      "base64", f_base64
      "base64ToBinary", f_base64ToBinary
      "base64ToString", f_base64ToString
      "binary", f_binary
      "decimal", f_decimal
      "json", f_json
      "string", f_string
      "sub", f_sub
      "outputs", f_outputs
      "trigger", f_trigger
      "variables", f_variables ]
    |> List.toSeq
    |> Seq.append conditions
    |> Map.ofSeq
