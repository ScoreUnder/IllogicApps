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

type NumberSubtype =
    | Integer of int
    | Float of float
    | Decimal of decimal

type Number2Subtype =
    | Integer2 of int * int
    | Float2 of float * float
    | Decimal2 of decimal * decimal

type Arithmetic2Type =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulo
    | Min
    | Max

let inline performArithmeticOp< ^a
    when ^a: (static member (+): ^a * ^a -> ^a)
    and ^a: (static member (-): ^a * ^a -> ^a)
    and ^a: (static member (*): ^a * ^a -> ^a)
    and ^a: (static member (/): ^a * ^a -> ^a)
    and ^a: (static member (%): ^a * ^a -> ^a)
    and ^a: (static member Min: ^a * ^a -> ^a)
    and ^a: (static member Max: ^a * ^a -> ^a)>
    op
    (a: 'a)
    (b: 'a)
    =
    match op with
    | Add -> a + b
    | Subtract -> a - b
    | Multiply -> a * b
    | Divide -> a / b
    | Modulo -> a % b
    | Min -> (^a: (static member Min: ^a * ^a -> ^a) (a, b))
    | Max -> (^a: (static member Max: ^a * ^a -> ^a) (a, b))

let jsonNumberToSubtype (node: JsonNode) : NumberSubtype =
    match node.GetValueKind() with
    | JsonValueKind.Number ->
        let value = box <| node.GetValue()

        match value with
        | :? int as i -> Integer i
        | :? float as f -> Float f
        | :? decimal as d -> Decimal d
        | _ -> failwithf "Expected number, got %A" (value.GetType().Name)
    | kind -> failwithf "Expected number, got %A" kind

let promoteNums a b =
    match a, b with
    | Integer a, Integer b -> Integer2(a, b)
    | Integer a, Float b -> Float2(float a, b)
    | Integer a, Decimal b -> Decimal2(decimal a, b)
    | Float a, Integer b -> Float2(a, float b)
    | Float a, Float b -> Float2(a, b)
    | Float a, Decimal b -> Decimal2(decimal a, b)
    | Decimal a, Integer b -> Decimal2(a, decimal b)
    | Decimal a, Float b -> Decimal2(a, decimal b)
    | Decimal a, Decimal b -> Decimal2(a, b)

let arithmetic2 op num1 num2 =
    promoteNums (jsonNumberToSubtype num1) (jsonNumberToSubtype num2)
    |> function
        | Integer2(a, b) -> performArithmeticOp op a b |> JsonValue.Create
        | Float2(a, b) -> performArithmeticOp op a b |> JsonValue.Create
        | Decimal2(a, b) -> performArithmeticOp op a b |> JsonValue.Create

let arithmetic2Function (op: Arithmetic2Type) (args: Args) : JsonNode =
    match args with
    | [ a; b ] ->
        match a.GetValueKind(), b.GetValueKind() with
        | JsonValueKind.Number, JsonValueKind.Number -> arithmetic2 op a b
        | kindA, kindB -> failwithf "Expected numbers, got %A and %A" kindA kindB
    | _ -> failwith "Expected 2 arguments"

let arrayReduceArithmetic2 op (args: Args) : JsonNode =
    let aux (args: Args) =
        match args with
        | [] -> failwith "Max of empty list"
        | [ a ] -> a
        | first :: rest ->
            rest
            |> List.fold
                (fun acc v ->
                    match promoteNums acc (jsonNumberToSubtype v) with
                    | Integer2(a, b) -> Integer(performArithmeticOp op a b)
                    | Float2(a, b) -> Float(performArithmeticOp op a b)
                    | Decimal2(a, b) -> Decimal(performArithmeticOp op a b))
                (jsonNumberToSubtype first)
            |> fun v -> JsonValue.Create(v)

    match args with
    | [ a ] when a.GetValueKind() = JsonValueKind.Array -> aux (Seq.toList (a.AsArray()))
    | _ :: _ :: _ -> aux args
    | _ -> failwith "Expected 2 or more arguments, or an array"

let myRand = lazy (System.Random())

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

let f_add _ (args: Args) : JsonNode = arithmetic2Function Add args
let f_div _ (args: Args) : JsonNode = arithmetic2Function Divide args
let f_max _ (args: Args) : JsonNode = arrayReduceArithmetic2 Max args
let f_min _ (args: Args) : JsonNode = arrayReduceArithmetic2 Min args
let f_mod _ (args: Args) : JsonNode = arithmetic2Function Modulo args
let f_mul _ (args: Args) : JsonNode = arithmetic2Function Multiply args

let f_rand _ (args: Args) : JsonNode =
    match args with
    | [ a; b ] ->
        match a.GetValueKind(), b.GetValueKind() with
        | JsonValueKind.Number, JsonValueKind.Number ->
            let a = a.GetValue<int>()
            let b = b.GetValue<int>()

            let result = a + myRand.Force().Next(b - a)
            JsonValue.Create(result)
        | kindA, kindB -> failwithf "Expected numbers, got %A and %A" kindA kindB
    | _ -> failwith "Expected 2 arguments"

let f_range _ (args: Args) : JsonNode =
    match args with
    | [ a; b ] ->
        match a.GetValueKind(), b.GetValueKind() with
        | JsonValueKind.Number, JsonValueKind.Number ->
            let a = a.GetValue<int>()
            let b = b.GetValue<int>()

            Array.init b (fun v -> JsonValue.Create(v + a): JsonNode)
            |> fun a -> new JsonArray(a)
        | kindA, kindB -> failwithf "Expected numbers, got %A and %A" kindA kindB
    | _ -> failwith "Expected 2 arguments"

let f_sub _ (args: Args) : JsonNode = arithmetic2Function Subtract args

// Date and time functions

let f_addDays _ (args: Args) : JsonNode =
    let addDays' (a: JsonNode) (b: JsonNode) fmt =
        match a.GetValueKind(), b.GetValueKind() with
        | JsonValueKind.String, JsonValueKind.Number ->
            let ts = a.GetValue<string>()
            let days = b.GetValue<int>()

            let dt = System.DateTimeOffset.Parse(ts)
            let result = dt.AddDays(float days)

            JsonValue.Create(result.ToString(fmt))
        | kindA, kindB -> failwithf "Expected string and number, got %A and %A" kindA kindB

    match args with
    | [ ts; days ] -> addDays' ts days "o"
    | [ ts; days; fmt ] -> addDays' ts days (fmt |> ensureString)
    | _ -> failwith "Expected 2 or 3 arguments"

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
      "add", f_add
      "div", f_div
      "max", f_max
      "min", f_min
      "mod", f_mod
      "mul", f_mul
      "rand", f_rand
      "range", f_range
      "sub", f_sub
      "outputs", f_outputs
      "trigger", f_trigger
      "variables", f_variables ]
    |> List.toSeq
    |> Seq.append conditions
    |> Map.ofSeq
