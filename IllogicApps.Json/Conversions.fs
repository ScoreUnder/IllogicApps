module IllogicApps.Json.Conversions

open System.Collections.Immutable
open System.IO

let escapeStringForJson str =
    let sb = System.Text.StringBuilder()

    for c in str do
        match c with
        | '\\' -> sb.Append("\\\\")
        | '\"' -> sb.Append("\\\"")
        | '\b' -> sb.Append("\\b")
        | '\f' -> sb.Append("\\f")
        | '\n' -> sb.Append("\\n")
        | '\r' -> sb.Append("\\r")
        | '\t' -> sb.Append("\\t")
        | _ when c < char 0x20 -> sb.AppendFormat("\\u{0:x4}", System.Convert.ToInt32(c))
        | _ -> sb.Append(c)
        |> ignore

    sb.ToString()

let stringOfJson json =
    let rec aux acc json =
        match json with
        | Null -> "null" :: acc
        | Array a when a.IsEmpty -> "[]" :: acc
        | Array a -> "[" :: List.tail (Seq.foldBack (fun el acc -> "," :: aux acc el) a ("]" :: acc))
        | Object o when OrderedMap.isEmpty o -> "{}" :: acc
        | Object o ->
            "{"
            :: List.tail (
                OrderedMap.foldBack
                    (fun k v acc -> "," :: "\"" :: (escapeStringForJson k) :: "\":" :: aux acc v)
                    o
                    ("}" :: acc)
            )
        | String s -> "\"" :: (escapeStringForJson s) :: "\"" :: acc
        | Integer i -> string i :: acc
        | Float f when System.Double.IsNaN f -> "\"NaN\"" :: acc
        | Float f when System.Double.IsNegativeInfinity f -> "\"-Infinity\"" :: acc
        | Float f when System.Double.IsPositiveInfinity f -> "\"Infinity\"" :: acc
        | Float f -> string f :: acc
        | Decimal d -> string d :: acc
        | Boolean true -> "true" :: acc
        | Boolean false -> "false" :: acc

    aux [] json |> String.concat ""

let rec writePrettyJson (writer: TextWriter) json =
    let rec write' json indent =
        match json with
        | Null -> writer.Write("null")
        | Array a when a.IsEmpty -> writer.Write("[]")
        | Array a when a.Length = 1 ->
            // Short array, don't indent
            writer.Write("[")
            write' a.[0] indent
            writer.Write("]")
        | Array a ->
            writer.WriteLine("[")

            let nextIndent = indent + "  "
            writer.Write(nextIndent)
            write' a.[0] nextIndent

            for el in a.AsSpan(System.Range.StartAt(1)) do
                writer.WriteLine(",")
                writer.Write(nextIndent)
                write' el nextIndent

            writer.WriteLine()
            writer.Write(indent)
            writer.Write("]")
        | Object o when OrderedMap.isEmpty o -> writer.Write("{}")
        | Object o when o.Count = 1 ->
            writer.Write("{")
            let (KeyValue(k, v)) = Seq.head o
            write' (String k) indent
            writer.Write(": ")
            write' v indent
            writer.Write("}")
        | Object o ->
            writer.WriteLine("{")

            let nextIndent = indent + "  "
            let asSeq = OrderedMap.toSeq o

            let k, v = Seq.head asSeq
            writer.Write(nextIndent)
            write' (String k) nextIndent
            writer.Write(": ")
            write' v nextIndent

            for k, v in Seq.skip 1 asSeq do
                writer.WriteLine(",")
                writer.Write(nextIndent)
                write' (String k) nextIndent
                writer.Write(": ")
                write' v nextIndent

            writer.WriteLine()
            writer.Write(indent)
            writer.Write("}")
        | j -> writer.Write(stringOfJson j)

    write' json ""
    writer.WriteLine()

let stringOfJsonType json =
    match json with
    | JsonType.Null -> "null"
    | JsonType.Array -> "array"
    | JsonType.Object -> "object"
    | JsonType.String -> "string"
    | JsonType.Integer -> "integer"
    | JsonType.Float -> "float"
    | JsonType.Decimal -> "decimal"
    | JsonType.Boolean -> "boolean"

let createArray seq = Array(ImmutableArray.CreateRange(seq))
let createObject seq = Object(OrderedMap.ofSeq seq)
let emptyArray = Array(ImmutableArray.Empty)
let emptyObject = Object(OrderedMap.empty)

let numberAsDecimal json =
    match json with
    | Decimal d -> d
    | Float f -> decimal f
    | Integer i -> decimal i
    | _ -> failwithf "Expected number, got %A" (JsonTree.getType json)

let numberAsFloat json =
    match json with
    | Decimal d -> float d
    | Float f -> f
    | Integer i -> float i
    | _ -> failwithf "Expected number, got %A" (JsonTree.getType json)

let (|NumbersAsDecimal|NumbersAsFloat|NumbersAsInteger|NotNumbers|) tuple =
    match tuple with
    | Decimal a, b -> NumbersAsDecimal(a, numberAsDecimal b)
    | a, Decimal b -> NumbersAsDecimal(numberAsDecimal a, b)
    | Float a, b -> NumbersAsFloat(a, numberAsFloat b)
    | a, Float b -> NumbersAsFloat(numberAsFloat a, b)
    | Integer a, Integer b -> NumbersAsInteger(a, b)
    | _ -> NotNumbers(tuple)

let ensureArray =
    function
    | Array a -> a
    | v -> failwithf "Expected array, got %A" (JsonTree.getType v)

let ensureObject =
    function
    | Object o -> o
    | v -> failwithf "Expected object, got %A" (JsonTree.getType v)

let ensureString =
    function
    | String s -> s
    | v -> failwithf "Expected string, got %A" (JsonTree.getType v)

let ensureInteger =
    function
    | Integer i -> i
    | v -> failwithf "Expected integer, got %A" (JsonTree.getType v)

let ensureBoolean =
    function
    | Boolean b -> b
    | v -> failwithf "Expected boolean, got %A" (JsonTree.getType v)

let rawStringOfJson json =
    match json with
    | String s -> s
    | Float f when System.Double.IsNaN f -> "NaN"
    | Float f when System.Double.IsNegativeInfinity f -> "-Infinity"
    | Float f when System.Double.IsPositiveInfinity f -> "Infinity"
    | _ -> stringOfJson json

let optionOfJson json =
    match json with
    | Null -> None
    | json -> Some json

let jsonOfOption opt =
    match opt with
    | None -> Null
    | Some json -> json

let jsonOfStringsMap (map: OrderedMap<string, string>) =
    OrderedMap.mapValuesOnly JsonTree.String map |> JsonTree.Object

let stringsMapOfJson json =
    json |> ensureObject |> OrderedMap.mapValuesOnly ensureString

let jsonOfStringList list =
    list |> List.map JsonTree.String |> createArray

let stringListOfJson json =
    json |> ensureArray |> Seq.map ensureString |> List.ofSeq
