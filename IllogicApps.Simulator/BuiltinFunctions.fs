module IllogicApps.Simulator.BuiltinFunctions

open System.Collections.Generic
open System.Globalization
open System.IO
open System.Runtime.Serialization.Json
open System.Text.Json
open System.Text.Json.Nodes
open System.Xml
open System.Xml.Linq
open IllogicApps.Core
open Helpers
open JsonUtil

type LanguageFunction = SimulatorContext -> JsonNode list -> JsonNode
type Args = JsonNode list

module ContentType =
    let Binary = "application/octet-stream"
    let Xml = "application/xml"

    let private CharsetEq = "charset="

    let getCharset (contentType: string) =
        let parts = contentType.Split(';')

        parts
        |> Array.tryPick (fun v ->
            let v = v.Trim()
            if v.StartsWith(CharsetEq) then Some v else None)
        |> Option.map _.Substring(CharsetEq.Length)
        |> Option.map System.Text.Encoding.GetEncoding

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

let base64ToBlob (contentType: string) (base64: string) : JsonNode =
    new JsonObject(
        [ new KeyValuePair<string, JsonNode>("$content-type", JsonValue.Create(contentType))
          new KeyValuePair<string, JsonNode>("$content", JsonValue.Create(base64)) ]
    )

let strToBase64Blob (contentType: string) (str: string) : JsonNode =
    str |> toBase64 |> base64ToBlob contentType

let toBinary (str: string) : JsonNode =
    str |> strToBase64Blob "application/octet-stream"

let (|Base64StringBlob|_|) (node: JsonNode) : (string * byte array) option =
    if node = null then
        None
    else
        match node.GetValueKind() with
        | JsonValueKind.Object ->
            let node = node.AsObject()

            if node.ContainsKey("$content-type") && node.ContainsKey("$content") then
                let content = node["$content"].ToString() |> System.Convert.FromBase64String
                Some(node["$content-type"].ToString(), content)
            else
                None
        | _ -> None

let decodeByContentType contentType (content: byte array) =
    ContentType.getCharset contentType
    |> Option.defaultValue System.Text.Encoding.UTF8
    |> _.GetString(content)

let (|StringOrEncodedString|_|) (node: JsonNode) : string option =
    match node with
    | null -> None
    | Base64StringBlob(contentType, content) -> Some(decodeByContentType contentType content)
    | _ ->
        match node.GetValueKind() with
        | JsonValueKind.String -> Some(node.GetValue<string>())
        | _ -> None

let objectToString (node: JsonNode) : string =
    match node with
    | null -> ""
    | Base64StringBlob(contentType, content) -> decodeByContentType contentType content
    | n ->
        match n.GetValueKind() with
        | JsonValueKind.True -> "True"
        | JsonValueKind.False -> "False"
        | JsonValueKind.Null -> ""
        | JsonValueKind.Array
        | JsonValueKind.Object -> n.ToJsonString(sensibleSerialiserOptions)
        | _ -> n.ToString()

let isXmlContentType (contentType: string) =
    $"{contentType};"
        .StartsWith($"{ContentType.Xml};", System.StringComparison.OrdinalIgnoreCase)

let isBinaryContentType (contentType: string) =
    $"{contentType};"
        .StartsWith($"{ContentType.Binary};", System.StringComparison.OrdinalIgnoreCase)

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
    | Add -> Operators.Checked.(+) a b
    | Subtract -> Operators.Checked.(-) a b
    | Multiply -> Operators.Checked.(*) a b
    | Divide -> a / b
    | Modulo -> a % b
    | Min -> (^a: (static member Min: ^a * ^a -> ^a) (a, b))
    | Max -> (^a: (static member Max: ^a * ^a -> ^a) (a, b))

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
            |> numberSubtypeToJson

    match args with
    | [ a ] when a.GetValueKind() = JsonValueKind.Array -> aux (Seq.toList (a.AsArray()))
    | _ :: _ :: _ -> aux args
    | _ -> failwith "Expected 2 or more arguments, or an array"

let myRand = lazy (System.Random())

let dateTimeFunc2f (f: System.DateTimeOffset -> int -> System.DateTimeOffset) (args: Args) : JsonNode =
    let dateTimeFunc2f' (a: JsonNode) (b: JsonNode) (fmt: string) =
        match a.GetValueKind(), b.GetValueKind() with
        | JsonValueKind.String, JsonValueKind.String ->
            let ts1 = a.GetValue<string>()
            let val2 = b.GetValue<int>()

            let dt1 = System.DateTimeOffset.Parse(ts1)
            let result = f dt1 val2

            JsonValue.Create(result.ToString(fmt))
        | kindA, kindB -> failwithf "Expected string and integer, got %A and %A" kindA kindB

    match args with
    | [ ts1; ts2 ] -> dateTimeFunc2f' ts1 ts2 "o"
    | [ ts1; ts2; fmt ] -> dateTimeFunc2f' ts1 ts2 (fmt |> ensureString)
    | _ -> failwith "Expected 2 or 3 arguments"

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

// Logical comparison functions

let f_not _ (args: Args) : JsonNode =
    expectArgs 1 args
    let value = List.head args

    match value.GetValueKind() with
    | JsonValueKind.False -> JsonValue.Create(true)
    | JsonValueKind.True -> JsonValue.Create(false)
    | kind -> failwithf "Expected boolean, got %A" kind

// Conversion functions

let f_array _ (args: Args) : JsonNode =
    expectArgs 1 args

    safeClone <| List.head args

let f_base64 _ (args: Args) : JsonNode =
    expectArgs 1 args
    let str = ensureString <| List.head args

    JsonValue.Create(toBase64 str)

let f_base64ToBinary _ (args: Args) : JsonNode =
    expectArgs 1 args
    let str = ensureString <| List.head args

    // Re-generate the base64 string to ensure it's valid
    System.Convert.FromBase64String str
    |> System.Convert.ToBase64String
    |> base64ToBlob ContentType.Binary

let f_base64ToString _ (args: Args) : JsonNode =
    expectArgs 1 args
    let str = ensureString <| List.head args

    str |> fromBase64 |> (fun v -> JsonValue.Create(v))

let f_binary _ (args: Args) : JsonNode =
    match args with
    | [ a ] ->
        if a = null || a.GetValueKind() = JsonValueKind.Null then
            failwith "Expected non-null argument"

        toBinary (objectToString a)
    | _ -> failwith "Expected 1 argument"

let f_createArray _ (args: Args) : JsonNode =
    expectArgsAtLeast 1 args

    args |> Seq.map safeClone |> Seq.toArray |> JsonArray :> JsonNode

let f_decimal _ (args: Args) : JsonNode =
    expectArgs 1 args
    let str = List.head args |> _.ToString()

    match System.Decimal.TryParse(str, NumberStyles.Float ||| NumberStyles.Number, CultureInfo.InvariantCulture) with
    | true, result -> JsonValue.Create(result)
    | _ -> failwithf "Could not parse %s as decimal" str

let f_float _ (args: Args) : JsonNode =
    expectArgs 1 args
    let str = List.head args |> _.ToString()

    match System.Double.TryParse(str, NumberStyles.Float ||| NumberStyles.Number, CultureInfo.InvariantCulture) with
    | true, result -> JsonValue.Create(result)
    | _ -> failwithf "Could not parse %s as float" str

let f_int _ (args: Args) : JsonNode =
    expectArgs 1 args
    let str = List.head args |> _.ToString()

    match
        System.Int64.TryParse(str, NumberStyles.Float ||| NumberStyles.AllowThousands, CultureInfo.InvariantCulture)
    with
    | true, result -> JsonValue.Create(int64 (float result))
    | _ -> failwithf "Could not parse %s as int" str

let f_json _ (args: Args) : JsonNode =
    expectArgs 1 args

    let str =
        match args |> List.head with
        | Base64StringBlob(contentType, content) ->
            if isXmlContentType contentType then
                let xmlStr = decodeByContentType contentType content
                let stringWriter = new MemoryStream()

                try
                    XDocument
                        .Parse(xmlStr)
                        .WriteTo(JsonReaderWriterFactory.CreateJsonWriter(stringWriter))
                with ex ->
                    failwithf "Could not parse XML: %s\nDocument: %s\nOriginal: %s" ex.Message xmlStr (ex.ToString())

                stringWriter.ToArray() |> System.Text.Encoding.UTF8.GetString
            else if isBinaryContentType contentType then
                content |> decodeByContentType contentType
            else
                failwithf "Unknown content type %s" contentType
        | arg -> ensureString arg

    JsonNode.Parse(str)

let f_string _ (args: Args) : JsonNode =
    expectArgs 1 args
    JsonValue.Create(args |> List.head |> objectToString)

let f_xml _ (args: Args) : JsonNode =
    expectArgs 1 args

    match List.head args with
    | v when v.GetValueKind() = JsonValueKind.String ->
        let str = v.GetValue<string>()
        let doc = XmlDocument()
        doc.LoadXml(str)
        doc.OuterXml |> strToBase64Blob $"{ContentType.Xml};charset=utf-8"
    | Base64StringBlob(_, content) ->
        content
        |> System.Convert.ToBase64String
        |> base64ToBlob $"{ContentType.Xml};charset=utf-8"
    | v when v.GetValueKind() = JsonValueKind.Object ->
        let jsonBytes = System.Text.Encoding.UTF8.GetBytes(v.ToJsonString())

        let reader =
            JsonReaderWriterFactory.CreateJsonReader(jsonBytes, new XmlDictionaryReaderQuotas())

        let xml = XDocument.Load(reader)

        xml.ToString(SaveOptions.DisableFormatting)
        |> strToBase64Blob $"{ContentType.Xml};charset=utf-8"
    | v -> failwithf "Expected string or object, got %A" (v.GetValueKind())

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

let f_addDays _ (args: Args) : JsonNode = dateTimeFunc2f _.AddDays args
let f_addHours _ (args: Args) : JsonNode = dateTimeFunc2f _.AddHours args
let f_addMinutes _ (args: Args) : JsonNode = dateTimeFunc2f _.AddMinutes args
let f_addSeconds _ (args: Args) : JsonNode = dateTimeFunc2f _.AddSeconds args

let f_addToTime _ (args: Args) : JsonNode =
    let addToTime' (time: JsonNode) (interval: JsonNode) (unit: JsonNode) (format: string) =
        match time.GetValueKind(), interval.GetValueKind(), unit.GetValueKind() with
        | JsonValueKind.String, JsonValueKind.Number, JsonValueKind.String ->
            let datetime = time.GetValue<string>()
            let interval = interval.GetValue<int>()
            let unit = unit.GetValue<string>()

            let datetime = System.DateTimeOffset.Parse(datetime)

            let result =
                match unit.ToLowerInvariant() with
                | "second" -> datetime.AddSeconds(interval)
                | "minute" -> datetime.AddMinutes(interval)
                | "hour" -> datetime.AddHours(interval)
                | "day" -> datetime.AddDays(interval)
                | "week" -> datetime.AddDays(float interval * 7.0)
                | "month" -> datetime.AddMonths(interval)
                | "year" -> datetime.AddYears(interval)
                | _ -> failwithf "Unknown unit %s" unit

            JsonValue.Create(result.ToString(format))
        | kindA, kindB, kindC -> failwithf "Expected string, number, and string, got %A, %A, and %A" kindA kindB kindC

    match args with
    | [ ts1; ts2; unit ] -> addToTime' ts1 ts2 unit "o"
    | [ ts1; ts2; unit; fmt ] -> addToTime' ts1 ts2 unit (fmt |> ensureString)
    | _ -> failwith "Expected 3 or 4 arguments"

// Workflow functions

let f_outputs (sim: SimulatorContext) (args: Args) : JsonNode =
    expectArgs 1 args
    let actionName = ensureString <| List.head args

    match sim.GetActionResult actionName with
    | Some result -> result.Outputs |> Option.map _.DeepClone() |> optionToNull
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
      "array", f_array
      "base64", f_base64
      "base64ToBinary", f_base64ToBinary
      "base64ToString", f_base64ToString
      "binary", f_binary
      "createArray", f_createArray
      "decimal", f_decimal
      "float", f_float
      "int", f_int
      "json", f_json
      "string", f_string
      "xml", f_xml
      "add", f_add
      "div", f_div
      "max", f_max
      "min", f_min
      "mod", f_mod
      "mul", f_mul
      "rand", f_rand
      "range", f_range
      "sub", f_sub
      "addDays", f_addDays
      "addHours", f_addHours
      "addMinutes", f_addMinutes
      "addSeconds", f_addSeconds
      "addToTime", f_addToTime
      "outputs", f_outputs
      "trigger", f_trigger
      "variables", f_variables ]
    |> List.toSeq
    |> Seq.append conditions
    |> Map.ofSeq
