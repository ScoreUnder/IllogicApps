module IllogicApps.Json.Parser

open System.Collections.Immutable
open System.Globalization
open System.Text

type private ParserState =
    | ValueStart
    | ValueEnd of JsonTree
    | StringLiteral of StringBuilder
    | StringEscape of StringBuilder
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
    | ConstructingObject of (string * JsonTree) list
    | ConstructingObjectValue of string * (string * JsonTree) list
    | ConstructingArray of JsonTree list

let private parseFloat index (str: string) =
    match
        System.Double.TryParse(
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
    match System.Int64.TryParse(str, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
    | true, result -> Integer result
    | _ -> parseFloat index str

type JsonFormatException(message) =
    inherit System.FormatException(message)

let private fail (c: char) (index: int) (state: ParserState) (stack: ConstructingState list) =
    if index = -1 then
        raise (JsonFormatException(sprintf "Unexpected end of input in state %A with stack %A" state stack))
    else
        raise (
            JsonFormatException(
                sprintf "Unexpected character '%c' at index %d in state %A with stack %A" c index state stack
            )
        )

let private parseLiteralIdentifier s c index state stack =
    match s with
    | "null" -> Null
    | "true" -> Boolean true
    | "false" -> Boolean false
    | _ -> fail c index state stack

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
                match c with
                | ' '
                | '\t'
                | '\n'
                | '\r' -> parse' (index + 1) ValueStart stack
                | '"' -> parse' (index + 1) (StringLiteral(StringBuilder())) stack
                | '-' as c -> parse' (index + 1) (NumberZeroOrDigit(StringBuilder().Append(c))) stack
                | '0' as c -> parse' (index + 1) (NumberFractionDot(StringBuilder().Append(c))) stack
                | c when System.Char.IsAsciiDigit(c) ->
                    parse' (index + 1) (NumberDigit(StringBuilder().Append(c))) stack
                | '[' -> parse' (index + 1) ArrayStart stack
                | '{' -> parse' (index + 1) ObjectStart stack
                | 'n'
                | 't'
                | 'f' as c -> parse' (index + 1) (LiteralIdentifier(StringBuilder().Append(c))) stack
                | c -> fail c index state stack
            | ValueEnd v ->
                match c with
                | ' '
                | '\t'
                | '\n'
                | '\r' -> parse' (index + 1) state stack
                | ',' ->
                    match stack with
                    | ConstructingObjectValue(k, o) :: stack' ->
                        parse' (index + 1) ValueStart (ConstructingObject((k, v) :: o) :: stack')
                    | ConstructingArray a :: stack' ->
                        parse' (index + 1) ValueStart (ConstructingArray(v :: a) :: stack')
                    | _ -> fail c index state stack
                | ':' ->
                    match v, stack with
                    | String v, ConstructingObject k :: stack' ->
                        parse' (index + 1) ValueStart (ConstructingObjectValue(v, k) :: stack')
                    | _ -> fail c index state stack
                | ']' ->
                    match stack with
                    | ConstructingArray a :: stack' ->
                        parse' (index + 1) (ValueEnd(Array(ImmutableArray.ToImmutableArray(List.rev (v :: a))))) stack'
                    | _ -> fail c index state stack
                | '}' ->
                    match stack with
                    | ConstructingObjectValue(k, o) :: stack' ->
                        parse' (index + 1) (ValueEnd(Object(Map.ofList ((k, v) :: o)))) stack'
                    | _ -> fail c index state stack
                | _ -> fail c index state stack
            | StringLiteral sb ->
                match c with
                | '\\' -> parse' (index + 1) (StringEscape sb) stack
                | '"' -> parse' (index + 1) (ValueEnd(String(sb.ToString()))) stack
                | c when c < char 0x20 -> fail c index state stack
                | c -> parse' (index + 1) (StringLiteral(sb.Append(c))) stack
            | StringEscape sb ->
                match c with
                | '"'
                | '\\'
                | '/' as c -> parse' (index + 1) (StringLiteral(sb.Append(c))) stack
                | 'b' -> parse' (index + 1) (StringLiteral(sb.Append('\b'))) stack
                | 'f' -> parse' (index + 1) (StringLiteral(sb.Append('\f'))) stack
                | 'n' -> parse' (index + 1) (StringLiteral(sb.Append('\n'))) stack
                | 'r' -> parse' (index + 1) (StringLiteral(sb.Append('\r'))) stack
                | 't' -> parse' (index + 1) (StringLiteral(sb.Append('\t'))) stack
                | 'u' when index + 4 < str.Length ->
                    let hex = str.Substring(index + 1, 4)
                    let c = char (System.Int32.Parse(hex, NumberStyles.HexNumber))
                    parse' (index + 5) (StringLiteral(sb.Append(c))) stack
                | c -> fail c index state stack
            | NumberZeroOrDigit sb ->
                match c with
                | '0' as c -> parse' (index + 1) (NumberFractionDot(sb.Append(c))) stack
                | c when System.Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberDigit(sb.Append(c))) stack
                | _ -> fail c index state stack
            | NumberDigit sb ->
                match c with
                | c when System.Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberDigit(sb.Append(c))) stack
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
                | c when System.Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberFraction(sb.Append(c))) stack
                | _ -> fail c index state stack
            | NumberFraction sb ->
                match c with
                | c when System.Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberFraction(sb.Append(c))) stack
                | 'e'
                | 'E' as c -> parse' (index + 1) (NumberExponentSign(sb.Append(c))) stack
                | _ -> parse' index (ValueEnd(parseFloat index (sb.ToString()))) stack
            | NumberExponentSign sb ->
                match c with
                | '+'
                | '-' as c -> parse' (index + 1) (NumberExponentDigitStart(sb.Append(c))) stack
                | c when System.Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberExponentDigit(sb.Append(c))) stack
                | _ -> fail c index state stack
            | NumberExponentDigitStart sb ->
                match c with
                | c when System.Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberExponentDigit(sb.Append(c))) stack
                | _ -> fail c index state stack
            | NumberExponentDigit sb ->
                match c with
                | c when System.Char.IsAsciiDigit(c) -> parse' (index + 1) (NumberExponentDigit(sb.Append(c))) stack
                | _ -> parse' index (ValueEnd(parseFloat index (sb.ToString()))) stack
            | ObjectStart ->
                match c with
                | ' '
                | '\t'
                | '\n'
                | '\r' -> parse' (index + 1) ObjectStart stack
                | '"' ->
                    parse'
                        (index + 1)

                        (StringLiteral(StringBuilder()))
                        (ConstructingObject [] :: stack)
                | '}' -> parse' (index + 1) (ValueEnd(Object(Map.empty))) stack
                | _ -> fail c index state stack
            | ArrayStart ->
                match c with
                | ' '
                | '\t'
                | '\n'
                | '\r' -> parse' (index + 1) ArrayStart stack
                | ']' -> parse' (index + 1) (ValueEnd(Array(ImmutableArray.Empty))) stack
                | _ -> parse' index ValueStart (ConstructingArray [] :: stack)
            | LiteralIdentifier sb ->
                match c with
                | c when System.Char.IsAsciiLetter(c) -> parse' (index + 1) (LiteralIdentifier(sb.Append(c))) stack
                | _ -> parse' index (ValueEnd(parseLiteralIdentifier (sb.ToString()) c index state stack)) stack

    parse' 0 ValueStart []
