module IllogicApps.Json.JsonParser

open System
open System.Collections.Immutable
open System.Globalization
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
open System.Text

type private ParserState =
    | ValueStart
    | ValueEnd of JsonTree
    | StringLiteral
    | NumberZeroOrDigit of StringBuilder
    | NumberDigit of StringBuilder
    | NumberFractionDot of StringBuilder
    | NumberFractionStart of StringBuilder
    | NumberFraction of StringBuilder
    | NumberExponentSign of StringBuilder
    | NumberExponentDigitStart of StringBuilder
    | NumberExponentDigit of StringBuilder
    | ObjectStart
    | ArrayStart
    | LiteralIdentifier of StringBuilder

type private ConstructingState =
    | ConstructingObject of OrderedMap.Builder<string, JsonTree>
    | ConstructingObjectValue of string * OrderedMap.Builder<string, JsonTree>
    | ConstructingArray of JsonTree list

type JsonFormatException(message) =
    inherit FormatException(message)

let private parseFloat index (str: string) =
    match
        Double.TryParse(
            str,
            NumberStyles.AllowLeadingSign
            ||| NumberStyles.AllowDecimalPoint
            ||| NumberStyles.AllowExponent,
            CultureInfo.InvariantCulture
        )
    with
    | true, result -> Float result
    | _ -> failwithf "Unexpected number format '%s' at index %d" str index

let private parseInteger index (str: string) =
    match Int64.TryParse(str, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
    | true, result -> Integer result
    | _ -> parseFloat index str

let private debugChr (c: char) =
    sprintf "'%c' (\\u%04x)" c (int c)

let private fail (c: char) (index: int) (state: ParserState) (stack: ConstructingState list) =
    if index = -1 then
        match state, stack with
        | _, ConstructingObjectValue _ :: _ -> "Unexpected end of input in object. Missing closing brace."
        | _, ConstructingObject _ :: _ -> "Unexpected end of input in object. Missing value and closing brace."
        | _, ConstructingArray _ :: _ -> "Unexpected end of input in array. Missing closing bracket."
        | _ -> sprintf "Unexpected end of input in state %O with stack %O" state stack
        |> JsonFormatException
        |> raise
    else
        let charStr = debugChr c
        match state, stack with
        | ValueEnd _, ConstructingArray _ :: _ ->
            sprintf "Unexpected character %s at index %d: Expecting comma or closing bracket." charStr index
        | ValueEnd _, ConstructingObjectValue _ :: _ ->
            sprintf "Unexpected character %s at index %d: Expecting comma or closing brace." charStr index
        | ValueEnd _, ConstructingObject _ :: _ ->
            sprintf "Unexpected character %s at index %d: Expecting colon." charStr index
        | _ -> sprintf "Unexpected character %s at index %d in state %O with stack %O" charStr index state stack
        |> JsonFormatException
        |> raise

let private failBuildingString
    (sb: StringBuilder)
    (c: char)
    (index: int)
    (isEscape: bool)
    (stack: ConstructingState list)
    =
    match c with
    | _ when index = -1 ->
        sprintf
            "Unexpected end of input in string literal at index %d. Missing closing quote. String so far: %O"
            index
            sb
    | c when c < char 0x20 ->
        let charStr = debugChr c
        sprintf "Unexpected character %s at index %d in string literal. String so far: %O" charStr index sb
    | _ ->
        let charStr = debugChr c
        sprintf
            "Unexpected character %s when parsing %s at index %d with stack %O. String so far: %O"
            charStr
            (if isEscape then "string" else "escape sequence")
            index
            stack
            sb
    |> JsonFormatException
    |> raise

let private parseLiteralIdentifier s c index state stack =
    match s with
    | "null" -> Null
    | "true" -> JsonTree.Boolean true
    | "false" -> JsonTree.Boolean false
    | _ -> fail c index state stack

#nowarn "9" // FS0009: Uses of this construct may result in the generation of unverifiable .NET IL code.

let inline private unsafeGetVectorOfString (str: char ReadOnlySpan) =
    use addr = fixed &MemoryMarshal.GetReference(str)
    Unsafe.ReadUnaligned<Vector256<int16>>(FSharp.NativeInterop.NativePtr.toVoidPtr addr)

let inline private countMatches matchesVector =
    if X86.Avx2.IsSupported then
        matchesVector
        |> Vector256.AsByte
        |> X86.Avx2.MoveMask
        |> Int32.TrailingZeroCount
        |> fun v -> v >>> 1
    else
        let mask = Vector256.Create(
            0x0001s, 0x0002s, 0x0004s, 0x0008s, 0x0010s, 0x0020s, 0x0040s, 0x0080s,
            0x0100s, 0x0200s, 0x0400s, 0x0800s, 0x1000s, 0x2000s, 0x4000s, 0x8000s)

        Vector256.BitwiseAnd(mask, matchesVector)
        |> Vector256.Sum
        |> Int16.TrailingZeroCount
        |> int32

let private getNumStringPassthroughChars (str: char ReadOnlySpan) =
    let charVec = unsafeGetVectorOfString str

    Vector256.Equals(charVec, Vector256.Create(int16 '"'))
    |> (fun f -> Vector256.BitwiseOr(f, Vector256.Equals(charVec, Vector256.Create(int16 '\\'))))
    |> (fun f -> Vector256.BitwiseOr(f, Vector256.LessThan(charVec, Vector256.Create(int16 ' '))))
    |> countMatches

let private getNumWhitespaces (str: char ReadOnlySpan) =
    let charVec = unsafeGetVectorOfString str

    Vector256.Equals(charVec, Vector256.Create(int16 ' '))
    |> (fun f -> Vector256.BitwiseOr(f, Vector256.Equals(charVec, Vector256.Create(int16 '\t'))))
    |> (fun f -> Vector256.BitwiseOr(f, Vector256.Equals(charVec, Vector256.Create(int16 '\n'))))
    |> (fun f -> Vector256.BitwiseOr(f, Vector256.Equals(charVec, Vector256.Create(int16 '\r'))))
    |> Vector256.OnesComplement
    |> countMatches

[<CompiledName("Parse")>]
let parse (str: string) =
    let rec parse' (index: int) (state: ParserState) (stack: ConstructingState list) =
        if index = str.Length then
            match state, stack with
            | ValueEnd v, [] -> v
            | LiteralIdentifier v, [] -> parseLiteralIdentifier (v.ToString()) ' ' -1 state stack
            | (NumberDigit v | NumberFractionDot v), [] -> parseInteger index (v.ToString())
            | (NumberFraction v | NumberExponentDigit v), [] -> parseFloat index (v.ToString())
            | _ -> fail ' ' -1 state stack
        else
            let c = str.[index]

            match state with
            | ValueStart ->
                let index =
                    if index + Vector256<int16>.Count <= str.Length then
                        getNumWhitespaces (str.AsSpan(index)) + index
                    else
                        index

                if index = str.Length then
                    fail ' ' -1 state stack
                else
                    match str.[index] with
                    | ' '
                    | '\t'
                    | '\n'
                    | '\r' -> parse' (index + 1) ValueStart stack
                    | '"' -> parse' (index + 1) StringLiteral stack
                    | '-' as c -> parse' (index + 1) (NumberZeroOrDigit(StringBuilder().Append(c))) stack
                    | '0' as c -> parse' (index + 1) (NumberFractionDot(StringBuilder().Append(c))) stack
                    | c when Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberDigit(StringBuilder().Append(c))) stack
                    | '[' -> parse' (index + 1) ArrayStart stack
                    | '{' -> parse' (index + 1) ObjectStart stack
                    | 'n'
                    | 't'
                    | 'f' as c -> parse' (index + 1) (LiteralIdentifier(StringBuilder().Append(c))) stack
                    | c -> fail c index state stack
            | ValueEnd v ->
                let index =
                    if index + Vector256<int16>.Count <= str.Length then
                        getNumWhitespaces (str.AsSpan(index)) + index
                    else
                        index

                if index = str.Length then
                    match stack with
                    | [] -> v
                    | _ -> fail ' ' -1 state stack
                else
                    match str.[index] with
                    | ' '
                    | '\t'
                    | '\n'
                    | '\r' -> parse' (index + 1) state stack
                    | ',' as c ->
                        match stack with
                        | ConstructingObjectValue(k, o) :: stack' ->
                            parse' (index + 1) ValueStart (ConstructingObject(o.SetAtEnd(k, v)) :: stack')
                        | ConstructingArray a :: stack' ->
                            parse' (index + 1) ValueStart (ConstructingArray(v :: a) :: stack')
                        | _ -> fail c index state stack
                    | ':' as c ->
                        match v, stack with
                        | String v, ConstructingObject k :: stack' ->
                            parse' (index + 1) ValueStart (ConstructingObjectValue(v, k) :: stack')
                        | _ -> fail c index state stack
                    | ']' as c ->
                        match stack with
                        | ConstructingArray a :: stack' ->
                            parse'
                                (index + 1)
                                (ValueEnd(JsonTree.Array(ImmutableArray.ToImmutableArray(List.rev (v :: a)))))
                                stack'
                        | _ -> fail c index state stack
                    | '}' as c ->
                        match stack with
                        | ConstructingObjectValue(k, o) :: stack' ->
                            parse' (index + 1) (ValueEnd(JsonTree.Object(o.SetAtEnd(k, v).Build()))) stack'
                        | _ -> fail c index state stack
                    | c -> fail c index state stack
            | StringLiteral ->
                let sb = StringBuilder()

                let rec auxParse start index =
                    let index =
                        if index + Vector256<int16>.Count <= str.Length then
                            index + getNumStringPassthroughChars (str.AsSpan(index))
                        else
                            index

                    let finishStep () =
                        sb.Append(str.AsSpan(start, index - start)) |> ignore

                    if index = str.Length then
                        failBuildingString sb ' ' -1 false stack
                    else
                        match str.[index] with
                        | '\\' ->
                            finishStep ()
                            auxEscape (index + 1)
                        | '"' ->
                            finishStep ()
                            parse' (index + 1) (ValueEnd(JsonTree.String(sb.ToString()))) stack
                        | c when c < char 0x20 ->
                            finishStep ()
                            failBuildingString sb c index false stack
                        | _ -> auxParse start (index + 1)

                and startAuxParse i = auxParse i i

                and auxEscape index =
                    if index = str.Length then
                        failBuildingString sb ' ' -1 true stack
                    else
                        match str.[index] with
                        | '"'
                        | '\\'
                        | '/' as c ->
                            sb.Append(c) |> ignore
                            startAuxParse (index + 1)
                        | 'b' ->
                            sb.Append('\b') |> ignore
                            startAuxParse (index + 1)
                        | 'f' ->
                            sb.Append('\f') |> ignore
                            startAuxParse (index + 1)
                        | 'n' ->
                            sb.Append('\n') |> ignore
                            startAuxParse (index + 1)
                        | 'r' ->
                            sb.Append('\r') |> ignore
                            startAuxParse (index + 1)
                        | 't' ->
                            sb.Append('\t') |> ignore
                            startAuxParse (index + 1)
                        | 'u' when index + 4 < str.Length ->
                            let hex = str.Substring(index + 1, 4)
                            let c = char (Int32.Parse(hex, NumberStyles.HexNumber))
                            sb.Append(c) |> ignore
                            startAuxParse (index + 5)
                        | c -> failBuildingString sb c index true stack

                startAuxParse index
            | NumberZeroOrDigit sb ->
                match c with
                | '0' as c -> parse' (index + 1) (NumberFractionDot(sb.Append(c))) stack
                | c when Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberDigit(sb.Append(c))) stack
                | _ -> fail c index state stack
            | NumberDigit sb ->
                match c with
                | c when Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberDigit(sb.Append(c))) stack
                | '.' as c -> parse' (index + 1) (NumberFractionStart(sb.Append(c))) stack
                | 'e'
                | 'E' as c -> parse' (index + 1) (NumberExponentSign(sb.Append(c))) stack
                | _ -> parse' index (ValueEnd(parseInteger index (sb.ToString()))) stack
            | NumberFractionDot sb ->
                match c with
                | '.' as c -> parse' (index + 1) (NumberFractionStart(sb.Append(c))) stack
                | _ -> parse' index (ValueEnd(parseInteger index (sb.ToString()))) stack
            | NumberFractionStart sb ->
                match c with
                | c when Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberFraction(sb.Append(c))) stack
                | _ -> fail c index state stack
            | NumberFraction sb ->
                match c with
                | c when Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberFraction(sb.Append(c))) stack
                | 'e'
                | 'E' as c -> parse' (index + 1) (NumberExponentSign(sb.Append(c))) stack
                | _ -> parse' index (ValueEnd(parseFloat index (sb.ToString()))) stack
            | NumberExponentSign sb ->
                match c with
                | '+'
                | '-' as c -> parse' (index + 1) (NumberExponentDigitStart(sb.Append(c))) stack
                | c when Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberExponentDigit(sb.Append(c))) stack
                | _ -> fail c index state stack
            | NumberExponentDigitStart sb ->
                match c with
                | c when Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberExponentDigit(sb.Append(c))) stack
                | _ -> fail c index state stack
            | NumberExponentDigit sb ->
                match c with
                | c when Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberExponentDigit(sb.Append(c))) stack
                | _ -> parse' index (ValueEnd(parseFloat index (sb.ToString()))) stack
            | ObjectStart ->
                let index =
                    if index + Vector256<int16>.Count <= str.Length then
                        getNumWhitespaces (str.AsSpan(index)) + index
                    else
                        index

                if index = str.Length then
                    fail ' ' -1 state stack
                else
                    match str.[index] with
                    | ' '
                    | '\t'
                    | '\n'
                    | '\r' -> parse' (index + 1) ObjectStart stack
                    | '"' -> parse' (index + 1) StringLiteral (ConstructingObject(OrderedMap.Builder()) :: stack)
                    | '}' -> parse' (index + 1) (ValueEnd(JsonTree.Object(OrderedMap.empty))) stack
                    | c -> fail c index state stack
            | ArrayStart ->
                let index =
                    if index + Vector256<int16>.Count <= str.Length then
                        getNumWhitespaces (str.AsSpan(index)) + index
                    else
                        index

                if index = str.Length then
                    fail ' ' -1 state stack
                else
                    match str.[index] with
                    | ' '
                    | '\t'
                    | '\n'
                    | '\r' -> parse' (index + 1) ArrayStart stack
                    | ']' -> parse' (index + 1) (ValueEnd(JsonTree.Array(ImmutableArray.Empty))) stack
                    | _ -> parse' index ValueStart (ConstructingArray [] :: stack)
            | LiteralIdentifier sb ->
                match c with
                | c when Char.IsAsciiLetter(c) -> parse' (index + 1) (LiteralIdentifier(sb.Append(c))) stack
                | _ -> parse' index (ValueEnd(parseLiteralIdentifier (sb.ToString()) c index state stack)) stack

    parse' 0 ValueStart []
