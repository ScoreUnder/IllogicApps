module IllogicApps.Json.Conversions

open System.Collections.Immutable
open System.IO

let inline escapeStringForJson str =
    SerialisationHelper.escapeStringForJson str

let inline stringOfJson json = json.ToString()

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

let prettyStringOfJson json =
    use writer = new StringWriter()
    writePrettyJson writer json
    writer.ToString()

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
