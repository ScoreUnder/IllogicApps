module IllogicApps.Simulator.BuiltinFunctions

open System
open System.Globalization
open System.IO
open System.Net
open System.Runtime.Serialization.Json
open System.Text
open System.Xml
open System.Xml.Linq

open IllogicApps.Core
open IllogicApps.Json

type LanguageFunction = SimulatorContext -> JsonTree list -> JsonTree
type Args = JsonTree list

module ContentType =
    let Binary = "application/octet-stream"
    let Xml = "application/xml"
    let Json = "application/json"
    let Text = "text/plain"

    module Charset =
        let private CharsetEq = "charset="

        let Utf8 = "utf-8"

        let set (contentType: string) (charset: string) = $"{contentType};{CharsetEq}{charset}"

        let get (contentType: string) =
            let parts = contentType.Split(';')

            parts
            |> Array.tryPick (fun v ->
                let v = v.Trim()

                if v.StartsWith(CharsetEq, StringComparison.OrdinalIgnoreCase) then
                    Some v
                else
                    None)
            |> Option.map _.Substring(CharsetEq.Length)
            |> Option.map Encoding.GetEncoding

        let getBuggy (contentType: string) =
            let parts = contentType.Split(';', 3)

            match parts with
            | [| _; second; _ |]
            | [| _; second |] when second.StartsWith(CharsetEq, StringComparison.OrdinalIgnoreCase) ->
                Some(Encoding.GetEncoding(second.Substring CharsetEq.Length))
            | _ -> None

let expectArgs n (args: Args) =
    if args.Length <> n then
        failwithf "Expected %d arguments, got %d" n args.Length

let expectArgsRange min max (args: Args) =
    if args.Length < min || args.Length > max then
        failwithf "Expected between %d and %d arguments, got %d" min max args.Length

let expectArgsAtLeast n (args: Args) =
    if args.Length < n then
        failwithf "Expected at least %d arguments, got %d" n args.Length

let count seq el = Seq.filter ((=) el) seq |> Seq.length

let toBase64 (str: string) =
    str |> Encoding.UTF8.GetBytes |> Convert.ToBase64String

let fromBase64 (str: string) =
    str |> Convert.FromBase64String |> Encoding.UTF8.GetString

let base64ToBlob (contentType: string) (base64: string) : JsonTree =
    Conversions.createObject [ "$content-type", String contentType; "$content", String base64 ]

let strToBase64Blob (contentType: string) (str: string) : JsonTree =
    str |> toBase64 |> base64ToBlob contentType

let toBinary (str: string) : JsonTree =
    str |> strToBase64Blob ContentType.Binary

let (|Base64StringBlob|_|) (node: JsonTree) : (string * byte array) option =
    match node with
    | Object node ->
        if node.ContainsKey("$content-type") && node.ContainsKey("$content") then
            let content =
                node["$content"] |> Conversions.rawStringOfJson |> Convert.FromBase64String

            Some(Conversions.rawStringOfJson node["$content-type"], content)
        else
            None
    | _ -> None

let decodeByContentType (contentType: string) (content: byte array) =
    contentType.Split(";")
    |> Seq.skip 1
    |> Seq.iter (fun seg ->
        if count seg '=' = 0 then
            failwithf "Bad content type segment %s" seg)

    ContentType.Charset.get contentType
    |> Option.defaultValue Encoding.UTF8
    |> _.GetString(content)

let (|StringOrEncodedString|_|) (node: JsonTree) : string option =
    match node with
    | Base64StringBlob(contentType, content) -> Some(decodeByContentType contentType content)
    | String s -> Some s
    | _ -> None

let objectToString (node: JsonTree) : string =
    match node with
    | Base64StringBlob(contentType, content) -> decodeByContentType contentType content
    | Boolean true -> "True"
    | Boolean false -> "False"
    | Null -> ""
    | _ -> Conversions.rawStringOfJson node

let (|ContentTypedAny|_|) (node: JsonTree) =
    match node with
    | Base64StringBlob(contentType, content) -> Some(contentType, content)
    | Null -> None
    | String s -> Some(ContentType.Charset.set ContentType.Text ContentType.Charset.Utf8, Encoding.UTF8.GetBytes s)
    | n ->
        Some(
            ContentType.Charset.set ContentType.Json ContentType.Charset.Utf8,
            Encoding.UTF8.GetBytes(objectToString n)
        )

let isXmlContentType (contentType: string) =
    $"{contentType};"
        .StartsWith($"{ContentType.Xml};", StringComparison.OrdinalIgnoreCase)

let isBinaryContentType (contentType: string) =
    $"{contentType};"
        .StartsWith($"{ContentType.Binary};", StringComparison.OrdinalIgnoreCase)

type Arithmetic2Type =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulo
    | Min
    | Max

let inline performArithmeticOnNumber< ^a
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

let performArithmeticOnJson op num1 num2 =
    match num1, num2 with
    | Conversions.NumbersAsDecimal(a, b) -> Decimal(performArithmeticOnNumber op a b)
    | Conversions.NumbersAsFloat(a, b) -> Float(performArithmeticOnNumber op a b)
    | Conversions.NumbersAsInteger(a, b) -> Integer(performArithmeticOnNumber op a b)
    | _ -> failwithf "Expected numbers, got %A and %A" (JsonTree.getType num1) (JsonTree.getType num2)

let arithmetic2Function (op: Arithmetic2Type) (args: Args) : JsonTree =
    match args with
    | [ a; b ] -> performArithmeticOnJson op a b
    | _ -> failwith "Expected 2 arguments"

let arrayReduceArithmetic2 op (args: Args) : JsonTree =
    let aux (args: Args) =
        match args with
        | [] -> failwith "Max of empty list"
        | [ a ] -> a
        | first :: rest -> List.fold (performArithmeticOnJson op) first rest

    match args with
    | [ Array a ] -> aux (Seq.toList a)
    | _ :: _ :: _ -> aux args
    | _ -> failwith "Expected 2 or more arguments, or an array"

let myRand = lazy (Random())

let dateTimeFunc2f (f: DateTimeOffset -> int -> DateTimeOffset) (args: Args) : JsonTree =
    let dateTimeFunc2f' (a: JsonTree) (b: JsonTree) (fmt: string) =
        match a, b with
        | String ts1, Integer val2 ->
            let dt1 = DateTimeOffset.Parse(ts1)
            let result = f dt1 (int val2)

            String(result.ToString(fmt))
        | _ -> failwithf "Expected string and integer, got %A and %A" (JsonTree.getType a) (JsonTree.getType b)

    match args with
    | [ ts1; ts2 ] -> dateTimeFunc2f' ts1 ts2 "o"
    | [ ts1; ts2; fmt ] -> dateTimeFunc2f' ts1 ts2 (fmt |> Conversions.ensureString)
    | _ -> failwith "Expected 2 or 3 arguments"

let parseDataUri (str: string) isForString =
    if not (str.StartsWith("data:", StringComparison.OrdinalIgnoreCase)) then
        failwith "Not a data URI"

    let parts = str.Substring(5).Split(',', 2)

    if parts.Length <> 2 then
        failwith "Badly formed data URI (no comma)"

    let metaParts = parts.[0].Split(';')

    let contentTypeParts, text =
        if (Array.last metaParts).Equals("base64", StringComparison.OrdinalIgnoreCase) then
            let metaPartsWithoutBase64 =
                metaParts |> Seq.ofArray |> Seq.take (metaParts.Length - 1)

            metaPartsWithoutBase64, parts.[1]
        else
            metaParts, toBase64 (WebUtility.UrlDecode parts.[1])

    let contentType =
        match List.ofSeq contentTypeParts with
        | [] -> ContentType.Text
        | [ "" ] -> ContentType.Text
        | mime :: _ ->
            if isForString then
                if mime = "" then
                    failwith "Empty MIME type"
            else if count mime '/' <> 1 then
                failwithf "Badly formed MIME type: %s" mime

            String.concat ";" contentTypeParts

    contentType, text

// String functions

let f_concat _ (args: Args) : JsonTree =
    expectArgsAtLeast 1 args

    args |> List.map objectToString |> String.concat "" |> String

let f_startsWith _ (args: Args) : JsonTree =
    match args with
    | [ a; b ] ->
        match a, b with
        | String a, String b -> Boolean(a.StartsWith(b))
        | kindA, kindB -> failwithf "Expected strings, got %A and %A" kindA kindB
    | _ -> failwith "Expected 2 arguments"

// Collection functions

let f_item (sim: SimulatorContext) (args: Args) : JsonTree =
    expectArgs 0 args

    sim.ArrayOperationContext.Current

// Logical comparison functions

let f_not _ (args: Args) : JsonTree =
    expectArgs 1 args
    let value = List.head args

    match value with
    | Boolean b -> Boolean(not b)
    | kind -> failwithf "Expected boolean, got %A" kind

// Conversion functions

let f_array _ (args: Args) : JsonTree =
    expectArgs 1 args

    Conversions.createArray args

let f_base64 _ (args: Args) : JsonTree =
    expectArgs 1 args
    let str = Conversions.ensureString <| List.head args

    String(toBase64 str)

let f_base64ToBinary _ (args: Args) : JsonTree =
    expectArgs 1 args
    let str = Conversions.ensureString <| List.head args

    // Try decoding the base64 string to ensure it's valid
    Convert.FromBase64String str |> ignore

    str |> base64ToBlob ContentType.Binary

let f_base64ToString _ (args: Args) : JsonTree =
    expectArgs 1 args
    let str = Conversions.ensureString <| List.head args

    str |> fromBase64 |> String

let f_binary _ (args: Args) : JsonTree =
    match args with
    | [ Null ] -> failwith "Expected non-null argument"
    | [ a ] -> toBinary (objectToString a)
    | _ -> failwith "Expected 1 argument"

let f_createArray _ (args: Args) : JsonTree =
    expectArgsAtLeast 1 args

    Conversions.createArray args


let f_dataUri _ (args: Args) : JsonTree =
    expectArgs 1 args

    match List.head args with
    | ContentTypedAny(contentType, content) ->
        let base64 = Convert.ToBase64String(content)
        let dataUri = $"data:{contentType};base64,{base64}"
        String dataUri
    | _ -> failwith "Cannot convert to data URI"

let f_dataUriToBinary _ (args: Args) : JsonTree =
    expectArgs 1 args
    let str = Conversions.ensureString <| List.head args
    let contentType, text = parseDataUri str false

    contentType.Split(";")
    |> Seq.skip 1
    |> Seq.iter (fun seg ->
        if count seg '=' > 1 then
            failwithf "Bad data URI segment: %s" seg)

    ContentType.Charset.get contentType |> ignore // just want to throw if the charset is wrong

    base64ToBlob contentType text

let f_dataUriToString _ (args: Args) : JsonTree =
    expectArgs 1 args
    let str = Conversions.ensureString <| List.head args
    let contentType, text = parseDataUri str true

    if ContentType.Charset.getBuggy contentType |> Option.exists ((<>) Encoding.UTF8) then
        failwith "dataUriToString only supports utf-8 encoded text"

    String(fromBase64 text)

let f_decimal _ (args: Args) : JsonTree =
    expectArgs 1 args
    let str = List.head args |> Conversions.rawStringOfJson

    match System.Decimal.TryParse(str, NumberStyles.Number, CultureInfo.InvariantCulture) with
    | true, result -> Decimal result
    | _ -> failwithf "Could not parse %s as decimal" str

let f_float _ (args: Args) : JsonTree =
    let convertFrom, culture =
        match args with
        | [ s ] -> s, CultureInfo.DefaultThreadCurrentCulture
        | [ s; c ] -> s, CultureInfo.GetCultureInfo(Conversions.ensureString c)
        | _ -> failwithf "Expected 1 or 2 args, got %d" (List.length args)

    match convertFrom with
    | Float _ -> convertFrom
    | _ ->
        let str = Conversions.rawStringOfJson convertFrom

        if
            args.Length = 1
            && str.AsSpan().TrimEnd().EndsWith("infinity", StringComparison.OrdinalIgnoreCase)
        then
            failwithf "Could not parse %s as float (float('infinity') needs a locale)" str

        match Double.TryParse(str, NumberStyles.Float ||| NumberStyles.Number, culture) with
        | true, result -> Float result
        | _ -> failwithf "Could not parse %s as float" str

let f_int (sim: SimulatorContext) (args: Args) : JsonTree =
    let str, culture =
        match args with
        | [ s ] -> s, CultureInfo.DefaultThreadCurrentCulture
        | [ s; c ] -> s, CultureInfo.GetCultureInfo(Conversions.ensureString c)
        | _ -> failwithf "Expected 1 or 2 args, got %d" (List.length args)

    let str = Conversions.rawStringOfJson str

    if sim.IsBugForBugAccurate then
        match Double.TryParse(str, NumberStyles.Float ||| NumberStyles.AllowThousands, culture) with
        | true, result ->
            if Double.IsNaN result then
                Integer(int64 result)
            elif not (Double.IsInteger result) then
                failwithf "Could not parse %s as int" str
            else
                Integer(Operators.Checked.int64 result)
        | _ -> failwithf "Could not parse %s as int" str
    else
        match
            Int64.TryParse(
                str,
                NumberStyles.Integer
                ||| NumberStyles.AllowExponent
                ||| NumberStyles.AllowThousands,
                CultureInfo.InvariantCulture
            )
        with
        | true, result -> Integer result
        | _ -> failwithf "Could not parse %s as int" str

let f_isFloat _ (args: Args) : JsonTree =
    let str, culture =
        match args with
        | [ s ] -> s, CultureInfo.InvariantCulture
        | [ s; c ] -> s, CultureInfo.GetCultureInfo(Conversions.ensureString c)
        | _ -> failwithf "Expected 1 or 2 args, got %d" (List.length args)

    let str = Conversions.ensureString str

    match Double.TryParse(str, NumberStyles.Float ||| NumberStyles.Number, culture) with
    | v, _ -> Boolean v

let f_json (sim: SimulatorContext) (args: Args) : JsonTree =
    expectArgs 1 args

    match args |> List.head with
    | Base64StringBlob(contentType, content) ->
        if isXmlContentType contentType then
            let xmlStr = decodeByContentType contentType content
            use writer = new JsonXmlWriter(sim.IsBugForBugAccurate)

            try
                let doc = XmlDocument()
                doc.LoadXml(xmlStr)

                // Trigger an exception if the encoding is not supported
                match doc.FirstChild with
                | :? XmlDeclaration as decl ->
                    match decl.Encoding with
                    | "" -> ()
                    | e ->
                        if sim.IsBugForBugAccurate then
                            let encoding = Encoding.GetEncoding e

                            if
                                encoding = Encoding.UTF32
                                || encoding = Encoding.Unicode
                                || encoding = Encoding.BigEndianUnicode
                            then
                                failwithf "%s is not a supported encoding for no good reason lol" e
                | _ -> ()

                doc.WriteTo(writer)
            with ex ->
                failwithf "Could not parse XML: %s\nDocument: %s\nOriginal: %s" ex.Message xmlStr (ex.ToString())

            writer.Close()
            writer.Result
        else if isBinaryContentType contentType then
            content |> decodeByContentType contentType |> Parser.parse
        else
            failwithf "Unknown content type %s" contentType
    | arg -> Conversions.ensureString arg |> Parser.parse

let f_string _ (args: Args) : JsonTree =
    expectArgs 1 args
    String(args |> List.head |> objectToString)

let f_xml (sim: SimulatorContext) (args: Args) : JsonTree =
    expectArgs 1 args

    let makeXmlWith f =
        let doc = XmlDocument()
        f doc
        doc.OuterXml |> strToBase64Blob $"{ContentType.Xml};charset=utf-8"

    match List.head args with
    | String str -> makeXmlWith _.LoadXml(str)
    | Base64StringBlob(_, content) ->
        content
        |> Convert.ToBase64String
        |> base64ToBlob $"{ContentType.Xml};charset=utf-8"
    | Object _ as v -> makeXmlWith (JsonToXmlConversion.writeJsonToXml sim.IsBugForBugAccurate v)
    | v -> failwithf "Expected string or object, got %A" (JsonTree.getType v)

// Math functions

let f_add _ (args: Args) : JsonTree = arithmetic2Function Add args
let f_div _ (args: Args) : JsonTree = arithmetic2Function Divide args
let f_max _ (args: Args) : JsonTree = arrayReduceArithmetic2 Max args
let f_min _ (args: Args) : JsonTree = arrayReduceArithmetic2 Min args
let f_mod _ (args: Args) : JsonTree = arithmetic2Function Modulo args
let f_mul _ (args: Args) : JsonTree = arithmetic2Function Multiply args

let f_rand _ (args: Args) : JsonTree =
    expectArgs 2 args

    match args with
    | [ Integer a; Integer b ] ->
        let result = a + int64 (myRand.Force().Next(int32 (b - a)))
        Integer result
    | [ a; b ] -> failwithf "Expected numbers, got %A and %A" (JsonTree.getType a) (JsonTree.getType b)
    | _ -> failwith "Expected 2 arguments"

let f_range _ (args: Args) : JsonTree =
    match args with
    | [ Integer a; Integer b ] -> Seq.init (int b) (fun v -> Integer(int64 v + a)) |> Conversions.createArray
    | [ a; b ] -> failwithf "Expected numbers, got %A and %A" (JsonTree.getType a) (JsonTree.getType b)
    | _ -> failwith "Expected 2 arguments"

let f_sub _ (args: Args) : JsonTree = arithmetic2Function Subtract args

// Date and time functions

let f_addDays _ (args: Args) : JsonTree = dateTimeFunc2f _.AddDays args
let f_addHours _ (args: Args) : JsonTree = dateTimeFunc2f _.AddHours args
let f_addMinutes _ (args: Args) : JsonTree = dateTimeFunc2f _.AddMinutes args
let f_addSeconds _ (args: Args) : JsonTree = dateTimeFunc2f _.AddSeconds args

let f_addToTime _ (args: Args) : JsonTree =
    let addToTime' (time: JsonTree) (interval: JsonTree) (unit: JsonTree) (format: string) =
        match time, interval, unit with
        | String datetime, Integer interval, String unit ->
            let datetime = DateTimeOffset.Parse(datetime)
            let interval = int interval

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

            String(result.ToString(format))
        | kindA, kindB, kindC -> failwithf "Expected string, number, and string, got %A, %A, and %A" kindA kindB kindC

    match args with
    | [ ts1; ts2; unit ] -> addToTime' ts1 ts2 unit "o"
    | [ ts1; ts2; unit; fmt ] -> addToTime' ts1 ts2 unit (fmt |> Conversions.ensureString)
    | _ -> failwith "Expected 3 or 4 arguments"

// Workflow functions

let f_outputs (sim: SimulatorContext) (args: Args) : JsonTree =
    expectArgs 1 args
    let actionName = Conversions.ensureString <| List.head args

    match sim.GetActionResult actionName with
    | Some result -> result.outputs
    | None -> failwithf "Action %s not found" actionName

let f_trigger (sim: SimulatorContext) (args: Args) : JsonTree =
    expectArgs 0 args

    CompletedStepTypes.jsonOfCompletedTrigger sim.TriggerResult

let f_variables (sim: SimulatorContext) (args: Args) : JsonTree =
    expectArgs 1 args
    let variableName = Conversions.ensureString <| List.head args

    match sim.Variables.TryGetValue variableName with
    | true, value -> JsonUtil.illogicJsonOfSystemTextJson value
    | _ -> failwithf "Variable %s not found" variableName

// End function definitions

let private conditionToFunction (condition: BuiltinCondition.LanguageCondition) : LanguageFunction =
    fun _ -> condition >> Boolean

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
      "dataUri", f_dataUri
      "dataUriToBinary", f_dataUriToBinary
      "dataUriToString", f_dataUriToString
      "decimal", f_decimal
      "float", f_float
      "int", f_int
      "isFloat", f_isFloat
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
