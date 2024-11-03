module IllogicApps.Simulator.LanguageLexer

open System

type Token =
    | Identifier of string
    | String of string
    | Integer of int64
    | Number of float
    | OpenParen
    | CloseParen
    | OpenBracket
    | CloseBracket
    | Comma
    | Dot
    | QuestionMark

let isLiteralStringWithAtSign (rawStr: string) = rawStr.StartsWith("@@")

let requiresInterpolation (rawStr: string) =
    rawStr.Length > 1 && rawStr.StartsWith("@") || rawStr.Contains("@{")

let interpolationIsStringified (rawStr: string) =
    rawStr.StartsWith("@{") || not (rawStr.StartsWith("@"))

module Sublexers =
    let lexString start acc (remaining: string) =
        // Precondition: remaining starts with a single quote

        let rec takeStrParts (start: int) (acc: string list) (remaining: string) =
            let quoteIndex = remaining.IndexOf('\'')

            if quoteIndex = -1 then
                failwithf "Unterminated string"

            let myPart = remaining[.. quoteIndex - 1]
            let acc = myPart :: acc

            if quoteIndex = remaining.Length - 1 then
                start + remaining.Length, List.rev acc, ""
            elif remaining.[quoteIndex + 1] = '\'' then
                takeStrParts (start + quoteIndex + 2) acc remaining.[quoteIndex + 2 ..]
            else
                start + quoteIndex + 1, List.rev acc, remaining[quoteIndex + 1 ..]

        let nextStart, parts, remaining = takeStrParts start [] remaining.[1..]
        let str = String.Join("'", parts)
        (nextStart, (start, String str) :: acc, remaining)

    let isValidIdentifierChar (c: char) = Char.IsLetterOrDigit(c) || c = '_'

    let lexIdentifier start acc (remaining: string) =
        let lenOpt = remaining |> Seq.tryFindIndex (not << isValidIdentifierChar)

        match lenOpt with
        | Some len ->
            let nextStart = start + len
            let ident = remaining.[.. len - 1]
            nextStart, (start, Identifier ident) :: acc, remaining[len..]
        | None ->
            let nextStart = start + remaining.Length
            nextStart, (start, Identifier remaining) :: acc, ""

    type private NumberState =
        | Sign
        | Integral
        | Fractional
        | ExponentSign
        | Exponent

    let lexNumber start acc (remaining: string) =
        let rec takeNum (start: int) (state: NumberState) (remaining: string) =
            if remaining = "" then
                state, start
            else
                match state, remaining.[0] with
                | Sign, ('-' | '+') -> takeNum (start + 1) Integral remaining.[1..]
                | Sign, c when Char.IsDigit(c) -> takeNum (start + 1) Integral remaining.[1..]
                | Integral, c when Char.IsDigit(c) -> takeNum (start + 1) Integral remaining.[1..]
                | Integral, '.' -> takeNum (start + 1) Fractional remaining.[1..]
                | Fractional, c when Char.IsDigit(c) -> takeNum (start + 1) Fractional remaining.[1..]
                | (Fractional | Integral), ('e' | 'E') -> takeNum (start + 1) ExponentSign remaining.[1..]
                | ExponentSign, ('-' | '+') -> takeNum (start + 1) Exponent remaining.[1..]
                | (Exponent | ExponentSign), c when Char.IsDigit(c) -> takeNum (start + 1) Exponent remaining.[1..]
                | _ -> state, start

        let state, nextStart = takeNum start Sign remaining
        let len = nextStart - start
        let numStr = remaining.[.. len - 1]

        if numStr = "" then
            failwith
                "Invalid number (but this should never be reached because we have already checked the first character before calling lexNumber)"

        match numStr.[numStr.Length - 1] with
        | 'e'
        | 'E' -> failwith "Invalid number: trailing exponent marker"
        | '-'
        | '+' when state = ExponentSign -> failwith "Invalid number: trailing exponent sign"
        | _ when state = Sign -> failwith "Invalid number: no digits after sign"
        | _ -> ()

        match state with
        | Integral ->
            let num = Int64.Parse(numStr)
            nextStart, (start, Integer num) :: acc, remaining.[len..]
        | _ ->
            let num = Double.Parse(numStr)
            nextStart, (start, Number num) :: acc, remaining.[len..]

open Sublexers

let lex rawStr =
    let isStringified = interpolationIsStringified rawStr

    let rec lex' (start: int) (acc: (int * Token) list) (remaining: string) =
        if remaining = "" then
            if isStringified then
                failwithf "Unterminated string interpolation (position %d) in %s" start rawStr
            else
                start, List.rev acc, remaining
        elif remaining.[0] = '}' && isStringified then
            (start + 1), List.rev acc, remaining.[1..]
        else
            let nextStart, nextAcc, nextRemaining =
                try
                    match remaining.[0] with
                    | ' '
                    | '\t'
                    | '\n'
                    | '\r' -> (start + 1), acc, remaining.[1..] // Skip whitespace
                    | '\'' -> lexString start acc remaining
                    | '(' -> (start + 1), ((start, OpenParen) :: acc), remaining.[1..]
                    | ')' -> (start + 1), ((start, CloseParen) :: acc), remaining.[1..]
                    | '[' -> (start + 1), ((start, OpenBracket) :: acc), remaining.[1..]
                    | ']' -> (start + 1), ((start, CloseBracket) :: acc), remaining.[1..]
                    | ',' -> (start + 1), ((start, Comma) :: acc), remaining.[1..]
                    | '.' -> (start + 1), ((start, Dot) :: acc), remaining.[1..]
                    | '?' -> (start + 1), ((start, QuestionMark) :: acc), remaining.[1..]
                    | '-'
                    | '+' -> lexNumber start acc remaining
                    | '_' -> lexIdentifier start acc remaining
                    | c when Char.IsDigit(c) -> lexNumber start acc remaining
                    | c when Char.IsLetter(c) -> lexIdentifier start acc remaining
                    | _ -> failwithf "Unexpected character [%c]" remaining.[0]
                with
                | :? FormatException as ex when ex.GetType() = typeof<FormatException> ->
                    raise
                    <| new Exception(sprintf "Error: %s (position %d) in %s" ex.Message start rawStr, ex)
                | ex when ex.GetType() = typeof<Exception> ->
                    raise
                    <| new Exception(sprintf "Error: %s (position %d) in %s" ex.Message start rawStr, ex)

            lex' nextStart nextAcc nextRemaining

    let rec lexStringified (start: int) (acc: (int * Token) list list) (remaining: string) =
        let nextInterpolation = remaining.IndexOf("@{")

        if nextInterpolation = -1 then
            List.rev ([(start, String remaining)] :: acc)
        elif nextInterpolation <> 0 && remaining.[nextInterpolation - 1] = '@' then
            let str = remaining.[.. nextInterpolation - 1]
            let acc = [(start, String str)] :: acc
            let start = start + nextInterpolation + 1
            let remaining = remaining.[nextInterpolation + 1 ..]
            lexStringified start acc remaining
        else
            let str = remaining.[.. nextInterpolation - 1]
            let acc = [(start, String str)] :: acc
            let start = start + nextInterpolation + 2
            let remaining = remaining.[nextInterpolation + 2 ..]

            let start, lexed, remaining = lex' start [] remaining
            lexStringified start (lexed :: acc) remaining

    if isStringified then
        lexStringified 0 [] rawStr
    else
        let _, lexed, _ = lex' 1 [] rawStr.[1..]
        [lexed]
