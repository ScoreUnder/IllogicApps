module IllogicApps.Expression.Execution.BuiltinFunctions

open System
open System.Collections.Immutable
open System.Globalization
open System.Net
open System.Text
open System.Xml
open System.Xml.XPath

open IllogicApps.Core
open IllogicApps.Core.Support
open IllogicApps.Json

type Args = JsonTree list
type LanguageFunction = SimulatorContext -> Args -> JsonTree
type LazyArgs = JsonTree Lazy list
type LazyArgsLanguageFunction = SimulatorContext -> LazyArgs -> JsonTree

let expectArgs n (args: Args) =
    if args.Length <> n then
        failwithf "Expected %d arguments, got %d" n args.Length

let expectArgsRange min max (args: Args) =
    if args.Length < min || args.Length > max then
        failwithf "Expected between %d and %d arguments, got %d" min max args.Length

let expectArgsAtLeast n (args: Args) =
    if args.Length < n then
        failwithf "Expected at least %d arguments, got %d" n args.Length

let expectSingleArg (args: Args) =
    match args with
    | [ arg ] -> arg
    | _ -> failwithf "Expected single argument, got %d arguments" args.Length

let ensureStringMsg msg (node: JsonTree) =
    match node with
    | String s -> s
    | _ -> failwith msg

let count seq el = Seq.filter ((=) el) seq |> Seq.length

let (|StringOrEncodedString|_|) (node: JsonTree) : string option =
    match node with
    | Blob.Blob(contentType, content) -> Some(ContentType.Charset.decodeBytes contentType content)
    | String s -> Some s
    | _ -> None

let objectToString (node: JsonTree) : string =
    match node with
    | Blob.Blob(contentType, content) -> ContentType.Charset.decodeBytes contentType content
    | Boolean true -> "True"
    | Boolean false -> "False"
    | Float f -> string f
    | Decimal d -> string d
    | Null -> ""
    | _ -> Conversions.rawStringOfJson node

let (|ContentTypedAny|_|) (node: JsonTree) =
    match node with
    | Blob.Blob(contentType, content) -> Some(contentType, content)
    | Null -> None
    | String s -> Some(ContentType.TextUtf8, Encoding.UTF8.GetBytes s)
    | n -> Some(ContentType.JsonUtf8, Encoding.UTF8.GetBytes(objectToString n))

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
    | _ -> failwithf "Expected numbers, got %O and %O" (JsonTree.getType num1) (JsonTree.getType num2)

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

type ArrayOperation =
    | Union
    | Intersection

let performArrayOperation op (a: 'a ResizeArray) (b: 'a seq) =
    match op with
    | Union ->
        b
        |> Seq.iter (fun v ->
            if not (a.Contains v) then
                a.Add(v))
    | Intersection -> a.RemoveAll(fun v -> not (Seq.contains v b)) |> ignore

let performArrayOperationMany op (args: JsonTree list) =
    match args with
    | [] -> failwith "Expected at least one argument"
    | Array a :: tail ->
        let result = a |> Seq.distinct |> ResizeArray

        // TODO: (perf) this could be made faster for large tails by combining the tail args into a single set + list of order first
        tail
        |> List.iter (function
            | Array arg -> performArrayOperation op result arg
            | _ -> failwith "This function expects parameters of same type")

        Conversions.createArray result
    | Object o :: tail ->
        let result = o |> OrderedMap.toSeq |> ResizeArray

        tail
        |> List.iter (function
            | Object arg -> performArrayOperation op result (arg |> OrderedMap.toSeq)
            | _ -> failwith "This function expects parameters of same type")

        // Rebuild object with later pairs taking priority
        let builder = OrderedMap.Builder()
        result |> builder.SetRange |> _.Build() |> Object
    | _ -> failwith "Expected array or object arguments"

let myRand = lazy Random()

let dateTimeFunc2f (f: DateTime -> int -> DateTime) (args: Args) : JsonTree =
    let dateTimeFunc2f' (a: JsonTree) (b: JsonTree) (fmt: string) =
        match a, b with
        | String ts1, Integer val2 ->
            let dt1 = DateTime.Parse(ts1)
            let result = f dt1 (int val2)

            String(result.ToString(fmt))
        | _ -> failwithf "Expected string and integer, got %O and %O" (JsonTree.getType a) (JsonTree.getType b)

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

            metaPartsWithoutBase64, parts.[1] |> Base64.mark
        else
            metaParts, Base64.ofString (WebUtility.UrlDecode parts.[1])

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

let f_chunk _ (args: Args) : JsonTree =
    expectArgs 2 args

    match args with
    | [ String s; Integer i ] ->
        s
        |> Seq.chunkBySize (int i)
        |> Seq.map (string >> String)
        |> Conversions.createArray
    | [ Array a; Integer i ] ->
        a
        |> Seq.chunkBySize (int i)
        |> Seq.map Conversions.createArray
        |> Conversions.createArray
    | _ -> failwith "Expected string and integer, or array and integer"

let f_concat _ (args: Args) : JsonTree =
    expectArgsAtLeast 1 args

    args |> List.map objectToString |> String.concat "" |> String

let f_formatNumber _ (args: Args) : JsonTree =
    let num, format, locale =
        match args with
        | [ num; format ] -> num, format, CultureInfo.DefaultThreadCurrentCulture
        | [ num; format; locale ] -> num, format, CultureInfo.GetCultureInfo(Conversions.ensureString locale)
        | _ -> failwithf "Expected 2 or 3 args, got %d" (List.length args)

    let format = Conversions.ensureString format

    match num with
    | Integer i -> String(i.ToString(format, locale))
    | Float f -> String(f.ToString(format, locale))
    | Decimal d -> String(d.ToString(format, locale))
    | _ -> failwithf "Expected number, got %O" (JsonTree.getType num)

let f_guid _ (args: Args) : JsonTree =
    let format =
        match args with
        | [] -> "D"
        | [ String fmt ] -> fmt
        | [ _ ] -> failwith "Expected string argument"
        | _ -> failwith "Expected 0 or 1 argument"

    String(Guid.NewGuid().ToString(format))

let f_indexOf _ (args: Args) : JsonTree =
    match args with
    | [ String haystack; String needle ] -> haystack.IndexOf(needle) |> int64 |> Integer
    | [ a; b ] -> failwithf "Expected two strings; got %O and %O" (JsonTree.getType a) (JsonTree.getType b)
    | _ -> failwith "Expected two arguments"

let f_isFloat _ (args: Args) : JsonTree =
    let str, culture =
        match args with
        | [ s ] -> s, CultureInfo.InvariantCulture
        | [ s; c ] -> s, CultureInfo.GetCultureInfo(Conversions.ensureString c)
        | _ -> failwithf "Expected 1 or 2 args, got %d" (List.length args)

    let str = Conversions.ensureString str

    match Double.TryParse(str, NumberStyles.Float ||| NumberStyles.Number, culture) with
    | v, _ -> Boolean v

let f_isInt _ (args: Args) : JsonTree =
    expectArgs 1 args

    let str = Conversions.ensureString <| List.head args

    match
        Int64.TryParse(
            str,
            NumberStyles.Integer
            ||| NumberStyles.AllowExponent
            ||| NumberStyles.AllowThousands,
            CultureInfo.InvariantCulture
        )
    with
    | v, _ -> Boolean v

let f_lastIndexOf _ (args: Args) : JsonTree =
    match args with
    | [ String haystack; String needle ] -> haystack.LastIndexOf(needle) |> int64 |> Integer
    | [ a; b ] -> failwithf "Expected two strings; got %O and %O" (JsonTree.getType a) (JsonTree.getType b)
    | _ -> failwith "Expected two arguments"

let f_length _ (args: Args) : JsonTree =
    expectArgs 1 args

    match List.head args with
    | String s -> Integer(int64 s.Length)
    | Array a -> Integer(int64 a.Length)
    | _ -> failwith "Expected parameter to be an array or a string"

let f_nthIndexOf _ (args: Args) : JsonTree =
    expectArgs 3 args

    match args with
    | [ String haystack; String needle; Integer n ] ->
        if n = 0L then
            failwith "Expected the index parameter to be nonzero"

        if n > 0L then
            let rec nthIndexOf (haystack: string) (needle: string) (n: int) (start: int) =
                if n = 0 then
                    start
                else
                    match haystack.IndexOf(needle, start) with
                    | -1 -> -1
                    | i -> nthIndexOf haystack needle (n - 1) (i + 1)

            nthIndexOf haystack needle (int n) 0 |> int64 |> Integer
        else
            let rec nthLastIndexOf (haystack: string) (needle: string) (n: int) (start: int) =
                if n = 0 then
                    start
                else
                    match haystack.LastIndexOf(needle, start) with
                    | -1 -> -1
                    | i -> nthLastIndexOf haystack needle (n - 1) (i - 1)

            nthLastIndexOf haystack needle (int -n) (haystack.Length - 1)
            |> int64
            |> Integer
    | [ a; b; c ] ->
        failwithf
            "Expected three strings; got %O, %O, and %O"
            (JsonTree.getType a)
            (JsonTree.getType b)
            (JsonTree.getType c)
    | _ -> failwith "Expected three arguments"

let f_replace _ (args: Args) : JsonTree =
    match args with
    | [ String s; String search; String replace ] ->
        match search with
        | "" -> failwith "String cannot be of zero length. (Parameter 'oldValue')"
        | _ -> s.Replace(search, replace) |> String
    | [ a; b; c ] ->
        failwithf
            "Expected three strings; got %O, %O, and %O"
            (JsonTree.getType a)
            (JsonTree.getType b)
            (JsonTree.getType c)
    | _ -> failwith "Expected three arguments"

let f_slice _ (args: Args) : JsonTree =
    match args with
    | String text :: Integer startIndex :: etc ->
        let startIndex = Operators.Checked.int startIndex

        let endIndex =
            match etc with
            | [] -> text.Length
            | [ Integer endIndex ] -> Operators.Checked.int endIndex
            | _ -> failwith "Expected 2 or 3 arguments"

        let startIndex =
            if startIndex < 0 then
                startIndex + text.Length
            else
                startIndex

        let endIndex = if endIndex < 0 then endIndex + text.Length else endIndex

        if startIndex >= text.Length || endIndex < startIndex then
            String ""
        else
            let startIndex = max 0 startIndex
            let endIndex = min text.Length endIndex
            String(text.Substring(startIndex, endIndex - startIndex))
    | _ ->
        expectArgsRange 2 3 args
        failwith "Expected string and integer, or string, integer, and integer"

let f_split _ (args: Args) : JsonTree =
    match args with
    | [ String s; String separator ] ->
        s.Split([| separator |], StringSplitOptions.None)
        |> Seq.map String
        |> Conversions.createArray
    | [ _; _ ] -> failwith "Both arguments must be of type string"
    | _ -> failwith "Expected two arguments"

let f_substring _ (args: Args) : JsonTree =
    match args with
    | String s :: Integer startIndex :: etc ->
        let startIndex = Operators.Checked.int startIndex

        let length =
            match etc with
            | [] -> s.Length - startIndex
            | [ Integer length ] -> Operators.Checked.int length
            | [ invalid ] -> failwithf "Expected integer, got %O" (JsonTree.getType invalid)
            | _ -> failwith "Expected 2 or 3 arguments"

        if startIndex < 0 || startIndex + length > s.Length || length < 0 then
            failwith
                "Parameters out of range: 'start index' and 'length' must be non-negative integers and their sum must be no larger than the length of the string"

        String(s.Substring(startIndex, length))
    | _ ->
        expectArgsRange 2 3 args
        failwith "Expected string and integer, or string, integer, and integer"

let f_toLower _ (args: Args) : JsonTree =
    args
    |> expectSingleArg
    |> ensureStringMsg "Expected parameter to be a string"
    |> _.ToLowerInvariant()
    |> String

let f_toUpper _ (args: Args) : JsonTree =
    args
    |> expectSingleArg
    |> ensureStringMsg "Expected parameter to be a string"
    |> _.ToUpperInvariant()
    |> String

let f_trim _ (args: Args) : JsonTree =
    args
    |> expectSingleArg
    |> ensureStringMsg "The provided parameters are not valid"
    |> _.Trim()
    |> String

// Collection functions

let f_empty _ (args: Args) : JsonTree =
    match args with
    | [ Array a ] -> Boolean(a.Length = 0)
    | [ String s ] -> Boolean(s.Length = 0)
    | [ Object o ] -> Boolean(o.Count = 0)
    | [ Null ] -> Boolean true
    | [ _ ] -> failwith "Expected parameter to be an object, an array or a string"
    | _ -> failwith "This function expects one parameter"

let f_first _ (args: Args) : JsonTree =
    match args with
    | [ Array a ] -> if a.Length = 0 then Null else a.[0]
    | [ String s ] -> if s.Length = 0 then String "" else String(s.[0..0])
    | [ _ ] -> failwith "Expected parameter to be an array or a string"
    | _ -> failwith "This function expects one parameter"

let f_intersection _ (args: Args) : JsonTree =
    match args with
    | []
    | [ _ ] -> failwith "Expected at least two arguments"
    | _ -> performArrayOperationMany Intersection args

let f_item (sim: SimulatorContext) (args: Args) : JsonTree =
    expectArgs 0 args

    sim.ArrayOperationContext |> Option.map _.Current |> Conversions.jsonOfOption

let f_join _ (args: Args) : JsonTree =
    match args with
    | [ Array a; String separator ] -> a |> Seq.map objectToString |> String.concat separator |> String
    | [ Array _; _ ] -> failwith "Second argument must be of type string"
    | [ _; _ ] -> failwith "First argument must be an array"
    | _ -> failwith "This function expects two parameters"

let f_last _ (args: Args) : JsonTree =
    match args with
    | [ Array a ] -> if a.Length = 0 then Null else a.[a.Length - 1]
    | [ String s ] ->
        if s.Length = 0 then
            String ""
        else
            String(s.[s.Length - 1 ..])
    | [ _ ] -> failwith "Expected parameter to be an array or a string"
    | _ -> failwith "This function expects one parameter"

let f_union _ (args: Args) : JsonTree =
    match args with
    | []
    | [ _ ] -> failwith "Expected at least two arguments"
    | _ -> performArrayOperationMany Union args

// Logical comparison functions

let f_and _ (args: LazyArgs) : JsonTree =
    if args = [] then
        failwith "Expected at least one argument"

    Seq.forall
        (fun arg ->
            match arg with
            | Lazy(Boolean b) -> b
            | _ -> failwith "Expected boolean arguments")
        args
    |> Boolean

let f_if _ (args: LazyArgs) : JsonTree =
    match args with
    | [ Lazy(Boolean c); a; b ] -> if c then a.Value else b.Value
    | [ _; _; _ ] -> failwith "Expected boolean and two other arguments"
    | _ -> failwith "Expected three arguments"

let f_not _ (args: Args) : JsonTree =
    expectArgs 1 args
    let value = List.head args

    match value with
    | Boolean b -> Boolean(not b)
    | kind -> failwithf "Expected boolean, got %O" kind

let f_or _ (args: LazyArgs) : JsonTree =
    if args = [] then
        failwith "Expected at least one argument"

    Seq.exists
        (fun arg ->
            match arg with
            | Lazy(Boolean b) -> b
            | _ -> failwith "Expected boolean arguments")
        args
    |> Boolean

// Conversion functions

let f_array _ (args: Args) : JsonTree =
    expectArgs 1 args

    Conversions.createArray args

let f_base64 _ (args: Args) : JsonTree =
    expectArgs 1 args
    let str = Conversions.ensureString <| List.head args

    String(Base64.ofString str |> Base64.unmark)

let f_base64ToBinary _ (args: Args) : JsonTree =
    expectArgs 1 args
    let str = Conversions.ensureString <| List.head args

    // Try decoding the base64 string to ensure it's valid
    let str = Base64.mark str
    Base64.toBytes str |> ignore

    str |> Blob.ofBase64 ContentType.Binary

let f_base64ToString _ (args: Args) : JsonTree =
    expectArgs 1 args
    let str = Conversions.ensureString <| List.head args

    str |> Base64.mark |> Base64.toString |> String

let f_binary _ (args: Args) : JsonTree =
    match args with
    | [ Null ] -> failwith "Expected non-null argument"
    | [ a ] -> Blob.binaryOfString (objectToString a)
    | _ -> failwith "Expected 1 argument"

let f_bool _ (args: Args) : JsonTree =
    args
    |> expectSingleArg
    |> function
        | Integer 0L
        | Float 0.0
        | Decimal 0.0m -> Boolean false
        | Integer _
        | Float _
        | Decimal _ -> Boolean true
        | String s -> Boolean(System.Boolean.Parse(s))
        | Boolean b -> Boolean b
        | _ -> failwith "Expected number, string, or boolean"

let f_createArray _ (args: Args) : JsonTree =
    expectArgsAtLeast 1 args

    Conversions.createArray args

let f_dataUri _ (args: Args) : JsonTree =
    expectArgs 1 args

    match List.head args with
    | ContentTypedAny(contentType, content) ->
        let base64 = Base64.ofBytes content
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

    Blob.ofBase64 contentType text

let f_dataUriToString _ (args: Args) : JsonTree =
    expectArgs 1 args
    let str = Conversions.ensureString <| List.head args
    let contentType, text = parseDataUri str true

    if ContentType.Charset.getBuggy contentType |> Option.exists ((<>) Encoding.UTF8) then
        failwith "dataUriToString only supports utf-8 encoded text"

    String(Base64.toString text)

let f_decimal _ (args: Args) : JsonTree =
    expectArgs 1 args

    match List.head args with
    | Decimal _ as d -> d
    | Integer i -> Decimal(decimal i)
    | Float f -> Decimal(decimal f)
    | String str ->
        match System.Decimal.TryParse(str, NumberStyles.Number, CultureInfo.InvariantCulture) with
        | true, result -> Decimal result
        | _ -> failwithf "Could not parse %s as decimal" str
    | arg -> failwithf "Expected number or string, got %O" (JsonTree.getType arg)

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

let f_json (sim: SimulatorContext) (args: Args) : JsonTree =
    expectArgs 1 args

    match args |> List.head with
    | Blob.Blob(contentType, content) ->
        if ContentType.isXml contentType then
            let xmlStr = ContentType.Charset.decodeBytes contentType content
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
        else if ContentType.isBinary contentType then
            content |> ContentType.Charset.decodeBytes contentType |> Parser.parse
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
        doc.OuterXml |> Blob.ofString ContentType.XmlUtf8

    match List.head args with
    | String str -> makeXmlWith _.LoadXml(str)
    | Blob.Blob(_, content) -> content |> Base64.ofBytes |> Blob.ofBase64 ContentType.XmlUtf8
    | Object _ as v -> makeXmlWith (JsonToXmlConversion.writeJsonToXml sim.IsBugForBugAccurate v)
    | v -> failwithf "Expected string or object, got %O" (JsonTree.getType v)

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
    | [ a; b ] -> failwithf "Expected numbers, got %O and %O" (JsonTree.getType a) (JsonTree.getType b)
    | _ -> failwith "Expected 2 arguments"

let f_range _ (args: Args) : JsonTree =
    match args with
    | [ Integer a; Integer b ] -> Seq.init (int b) (fun v -> Integer(int64 v + a)) |> Conversions.createArray
    | [ a; b ] -> failwithf "Expected numbers, got %O and %O" (JsonTree.getType a) (JsonTree.getType b)
    | _ -> failwith "Expected 2 arguments"

let f_sub _ (args: Args) : JsonTree = arithmetic2Function Subtract args

// Date and time functions

let f_addDays _ (args: Args) : JsonTree = dateTimeFunc2f _.AddDays args
let f_addHours _ (args: Args) : JsonTree = dateTimeFunc2f _.AddHours args
let f_addMinutes _ (args: Args) : JsonTree = dateTimeFunc2f _.AddMinutes args
let f_addSeconds _ (args: Args) : JsonTree = dateTimeFunc2f _.AddSeconds args

let addToTime'' (datetime: string) (interval: int64) (unit: string) (format: string) =
    let datetime = DateTime.Parse(datetime)
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

let f_addToTime _ (args: Args) : JsonTree =
    let addToTime' (time: JsonTree) (interval: JsonTree) (unit: JsonTree) (format: string) =
        match time, interval, unit with
        | String datetime, Integer interval, String unit -> addToTime'' datetime interval unit format
        | kindA, kindB, kindC -> failwithf "Expected string, number, and string, got %O, %O, and %O" kindA kindB kindC

    match args with
    | [ ts1; ts2; unit ] -> addToTime' ts1 ts2 unit "o"
    | [ ts1; ts2; unit; fmt ] -> addToTime' ts1 ts2 unit (fmt |> Conversions.ensureString)
    | _ -> failwith "Expected 3 or 4 arguments"

let f_formatDateTime _ (args: Args) : JsonTree =
    let timestamp, format, locale =
        match args with
        | [ String timestamp ] -> timestamp, "o", "en-us"
        | [ String timestamp; String format ] -> timestamp, format, "en-us"
        | [ String timestamp; String format; String locale ] -> timestamp, format, locale
        | [ _ ]
        | [ _; _ ]
        | [ _; _; _ ] -> failwith "Expected all parameters to be strings"
        | _ -> failwith "Expected 1, 2, or 3 arguments"

    let timestamp = DateTime.Parse(timestamp)
    let locale = CultureInfo.GetCultureInfo(locale)

    String(timestamp.ToString(format, locale))

let f_getFutureTime _ (args: Args) : JsonTree =
    match args with
    | [ Integer interval; String unit ] -> addToTime'' (string DateTime.UtcNow) interval unit "o"
    | [ Integer interval; String unit; String format ] -> addToTime'' (string DateTime.UtcNow) interval unit format
    | _ -> failwith "Expected 2 or 3 arguments"

let f_getPastTime _ (args: Args) : JsonTree =
    match args with
    | [ Integer interval; String unit ] -> addToTime'' (string DateTime.UtcNow) (-interval) unit "o"
    | [ Integer interval; String unit; String format ] -> addToTime'' (string DateTime.UtcNow) (-interval) unit format
    | _ -> failwith "Expected 2 or 3 arguments"

let f_parseDateTime _ (args: Args) : JsonTree =
    let timestamp, locale, format =
        match args with
        | [ String timestamp ] -> timestamp, "en-us", None
        | [ String timestamp; String locale ] -> timestamp, locale, None
        | [ String timestamp; String locale; String format ] -> timestamp, locale, Some format
        | [ _ ]
        | [ _; _ ]
        | [ _; _; _ ] -> failwith "Expected all parameters to be strings"
        | _ -> failwith "Expected 1, 2, or 3 arguments"

    let locale = CultureInfo.GetCultureInfo(locale)

    match format with
    | Some format -> String(DateTime.ParseExact(timestamp, format, locale).ToString("o"))
    | None -> String(DateTime.Parse(timestamp, locale).ToString("o"))

let f_startOfDay _ (args: Args) : JsonTree =
    let timestamp, format =
        match args with
        | [ String timestamp ] -> timestamp, "o"
        | [ String timestamp; String format ] -> timestamp, format
        | [ _ ]
        | [ _; _ ] -> failwith "Expected string and optional string"
        | _ -> failwith "Expected 1 or 2 arguments"

    let timestamp = DateTime.Parse(timestamp, CultureInfo.InvariantCulture)
    timestamp.Date.ToString(format) |> String

let f_startOfHour _ (args: Args) : JsonTree =
    let timestamp, format =
        match args with
        | [ String timestamp ] -> timestamp, "o"
        | [ String timestamp; String format ] -> timestamp, format
        | [ _ ]
        | [ _; _ ] -> failwith "Expected string and optional string"
        | _ -> failwith "Expected 1 or 2 arguments"

    let timestamp = DateTime.Parse(timestamp, CultureInfo.InvariantCulture)
    timestamp.Date.AddHours(timestamp.Hour).ToString(format) |> String

let f_startOfMonth _ (args: Args) : JsonTree =
    let timestamp, format =
        match args with
        | [ String timestamp ] -> timestamp, "o"
        | [ String timestamp; String format ] -> timestamp, format
        | [ _ ]
        | [ _; _ ] -> failwith "Expected string and optional string"
        | _ -> failwith "Expected 1 or 2 arguments"

    let timestamp = DateTime.Parse(timestamp, CultureInfo.InvariantCulture)
    timestamp.Date.AddDays(float (1 - timestamp.Day)).ToString(format) |> String

let f_subtractFromTime _ (args: Args) : JsonTree =
    let subtractFromTime' (time: JsonTree) (interval: JsonTree) (unit: JsonTree) (format: string) =
        match time, interval, unit with
        | String datetime, Integer interval, String unit -> addToTime'' datetime (-interval) unit format
        | kindA, kindB, kindC -> failwithf "Expected string, number, and string, got %O, %O, and %O" kindA kindB kindC

    match args with
    | [ ts1; ts2; unit ] -> subtractFromTime' ts1 ts2 unit "o"
    | [ ts1; ts2; unit; fmt ] -> subtractFromTime' ts1 ts2 unit (fmt |> Conversions.ensureString)
    | _ -> failwith "Expected 3 or 4 arguments"

let f_ticks _ (args: Args) : JsonTree =
    args
    |> expectSingleArg
    |> Conversions.ensureString
    |> DateTimeOffset.Parse
    |> _.Ticks
    |> Integer

let f_utcNow _ (args: Args) : JsonTree =
    let format =
        match args with
        | [] -> "o"
        | [ String format ] -> format
        | [ _ ] -> failwith "Expected string argument"
        | _ -> failwith "Expected 0 or 1 argument"

    String(DateTime.UtcNow.ToString(format))

// Workflow functions

let f_actions (sim: SimulatorContext) (args: Args) : JsonTree =
    expectArgs 1 args
    let actionName = Conversions.ensureString <| List.head args

    match sim.GetActionResult actionName with
    | Some result -> result |> CompletedStepTypes.jsonOfCompletedAction
    | None -> failwithf "Action %s not found" actionName

let f_appsetting (sim: SimulatorContext) (args: Args) : JsonTree =
    args
    |> expectSingleArg
    |> Conversions.ensureString
    |> sim.GetAppConfig
    |> Option.map String
    |> Option.defaultValue Null

let f_body (sim: SimulatorContext) (args: Args) : JsonTree =
    f_actions sim args
    |> JsonTree.getKeyMapOrElse "outputs" (JsonTree.getKeyOrNull "body") (fun () -> Null)

let f_items (sim: SimulatorContext) (args: Args) : JsonTree =
    expectArgs 1 args
    let actionName = Conversions.ensureString <| List.head args

    match sim.GetArrayOperationContextByName actionName with
    | Some value -> value.Current
    | None -> failwithf "Array context action %s not found" actionName

let f_outputs (sim: SimulatorContext) (args: Args) : JsonTree =
    f_actions sim args |> JsonTree.getKeyOrNull "outputs"

let f_parameters (sim: SimulatorContext) (args: Args) : JsonTree =
    args
    |> expectSingleArg
    |> Conversions.ensureString
    |> sim.GetParameter
    |> Option.get

let f_trigger (sim: SimulatorContext) (args: Args) : JsonTree =
    expectArgs 0 args

    CompletedStepTypes.jsonOfCompletedAction sim.TriggerResult

let f_triggerBody (sim: SimulatorContext) (args: Args) : JsonTree =
    f_trigger sim args
    |> JsonTree.getKeyMapOrElse "outputs" (JsonTree.getKeyOrNull "body") (fun () -> Null)

let f_triggerOutputs (sim: SimulatorContext) (args: Args) : JsonTree =
    f_trigger sim args |> JsonTree.getKeyOrNull "outputs"

let f_variables (sim: SimulatorContext) (args: Args) : JsonTree =
    expectArgs 1 args
    let variableName = Conversions.ensureString <| List.head args

    match sim.GetVariable variableName with
    | Some value -> value
    | None -> failwithf "Variable %s not found" variableName

let f_workflow (sim: SimulatorContext) (args: Args) : JsonTree =
    expectArgs 0 args

    sim.WorkflowDetails |> ExternalServiceTypes.compatibleJsonOfWorkflowDetails

// Manipulation functions

let f_addProperty _ (args: Args) : JsonTree =
    match args with
    | [ Object o; String k; v ] ->
        let newMap = o |> OrderedMap.tryAddCaseInsensitive k v

        if newMap = o then
            let stringObj = o |> Object |> Conversions.stringOfJson
            failwithf "Expected property to not exist in the object: %s in %s" k stringObj
        else
            newMap |> Object
    | [ Object _; _; _ ] -> failwith "The second argument must be of type string"
    | [ _; _; _ ] -> failwith "The first argument must be of type object"
    | _ -> failwith "This function expects three parameters"

let f_coalesce _ (args: LazyArgs) : JsonTree =
    match args with
    | [] -> failwith "This function expects at least one parameter"
    | _ ->
        args
        |> List.tryPick (function
            | Lazy(Null) -> None
            | Lazy(x) -> Some x)
        |> Option.defaultValue Null

let f_removeProperty _ (args: Args) : JsonTree =
    match args with
    | [ Object o; String k ] -> o |> OrderedMap.tryRemoveCaseInsensitive k |> Object
    | [ Object _; _ ] -> failwith "The second argument must be of type string"
    | [ _; _ ] -> failwith "The first argument must be of type object"
    | _ -> failwith "This function expects two parameters"

let f_setProperty _ (args: Args) : JsonTree =
    match args with
    | [ Object o; String k; v ] -> o |> OrderedMap.setAtEndCaseInsensitive k v |> Object
    | [ Object _; _; _ ] -> failwith "The second argument must be of type string"
    | [ _; _; _ ] -> failwith "The first argument must be of type object"
    | _ -> failwith "This function expects three parameters"

let f_xpath _ (args: Args) : JsonTree =
    expectArgs 2 args

    match args with
    | [ Blob.Blob(contentType, xml); String xpath ] when ContentType.isXml contentType ->
        let xml = ContentType.Charset.decodeBytes contentType xml
        let doc = XmlDocument()
        doc.LoadXml(xml)

        match doc.CreateNavigator().Evaluate(xpath) with
        | :? XPathNodeIterator as iter ->
            iter
            |> Seq.cast<XPathNavigator>
            |> Seq.map (fun n ->
                match n.NodeType with
                | XPathNodeType.Text -> n.Value |> String
                | _ -> n.OuterXml |> Blob.ofString ContentType.XmlUtf8)
            |> Conversions.createArray
        | :? float as v -> Float v
        | :? string as v -> String v
        | :? bool as v -> Boolean v
        | v -> failwithf "XPath evaluation returned undocumented type %O" (v.GetType())
    | [ _; String _ ] -> failwith "Expected the first parameter to be an XML object"
    | _ -> failwith "Expected the second parameter to be a string"

// End function definitions

let private conditionToFunction (condition: BuiltinCondition.LanguageCondition) : LanguageFunction =
    fun _ -> condition >> Boolean

let functions: OrderedMap<string, LanguageFunction> =
    let conditions =
        BuiltinCondition.conditions
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> k, conditionToFunction v)

    [ "chunk", f_chunk
      "concat", f_concat
      "formatNumber", f_formatNumber
      "guid", f_guid
      "indexOf", f_indexOf
      "isFloat", f_isFloat
      "isInt", f_isInt
      "lastIndexOf", f_lastIndexOf
      "length", f_length
      "nthIndexOf", f_nthIndexOf
      "replace", f_replace
      "slice", f_slice
      "split", f_split
      "substring", f_substring
      "toLower", f_toLower
      "toUpper", f_toUpper
      "trim", f_trim
      "empty", f_empty
      "first", f_first
      "intersection", f_intersection
      "item", f_item
      "join", f_join
      "last", f_last
      "union", f_union
      "not", f_not
      "array", f_array
      "base64", f_base64
      "base64ToBinary", f_base64ToBinary
      "base64ToString", f_base64ToString
      "binary", f_binary
      "bool", f_bool
      "createArray", f_createArray
      "dataUri", f_dataUri
      "dataUriToBinary", f_dataUriToBinary
      "dataUriToString", f_dataUriToString
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
      "formatDateTime", f_formatDateTime
      "getFutureTime", f_getFutureTime
      "getPastTime", f_getPastTime
      "parseDateTime", f_parseDateTime
      "startOfDay", f_startOfDay
      "startOfHour", f_startOfHour
      "startOfMonth", f_startOfMonth
      "subtractFromTime", f_subtractFromTime
      "ticks", f_ticks
      "utcNow", f_utcNow
      "actions", f_actions
      "appsetting", f_appsetting
      "body", f_body
      "items", f_items
      "outputs", f_outputs
      "parameters", f_parameters
      "trigger", f_trigger
      "triggerBody", f_triggerBody
      "triggerOutputs", f_triggerOutputs
      "variables", f_variables
      "workflow", f_workflow
      "addProperty", f_addProperty
      "removeProperty", f_removeProperty
      "setProperty", f_setProperty
      "xpath", f_xpath ]
    |> List.toSeq
    |> Seq.append conditions
    |> OrderedMap.ofSeq

let lazyFunctions: OrderedMap<string, LazyArgsLanguageFunction> =
    [ "and", f_and; "if", f_if; "or", f_or; "coalesce", f_coalesce ]
    |> List.toSeq
    |> OrderedMap.ofSeq
