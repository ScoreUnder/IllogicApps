module IllogicApps.Simulator.LanguageLexer

open System

type Token =
    | Identifier of string
    | String of string
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
    rawStr.StartsWith("@") || rawStr.Contains("@{")

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
        let len = remaining |> Seq.findIndex (not << isValidIdentifierChar)
        let nextStart = start + len
        let ident = remaining.[.. len - 1]
        nextStart, (start, Identifier ident) :: acc, remaining[len..]

    type private NumberState =
        | Sign
        | Integral
        | Fractional
        | ExponentSign
        | Exponent

    let lexNumber start acc (remaining: string) =
        let rec takeNum (start: int) (state: NumberState) (remaining: string) =
            if remaining = "" then
                start
            else
                match state, remaining.[0] with
                | Sign, ('-' | '+') -> takeNum (start + 1) Integral remaining.[1..]
                | Sign, c when Char.IsDigit(c) -> takeNum (start + 1) Integral remaining.[1..]
                | Integral, c when Char.IsDigit(c) -> takeNum (start + 1) Integral remaining.[1..]
                | Integral, '.' -> takeNum (start + 1) Fractional remaining.[1..]
                | Fractional, c when Char.IsDigit(c) -> takeNum (start + 1) Fractional remaining.[1..]
                | Fractional, ('e' | 'E') -> takeNum (start + 1) ExponentSign remaining.[1..]
                | ExponentSign, ('-' | '+') -> takeNum (start + 1) Exponent remaining.[1..]
                | Exponent, c when Char.IsDigit(c) -> takeNum (start + 1) Exponent remaining.[1..]
                | _ -> start

        let nextStart = takeNum start Sign remaining
        let len = nextStart - start
        let numStr = remaining.[.. len - 1]
        let num = Double.Parse(numStr)
        nextStart, (start, Number num) :: acc, remaining.[len..]

open Sublexers

let lex rawStr =
    let isStringified = interpolationIsStringified rawStr

    let rec lex' (start: int) (acc: (int * Token) list) (remaining: string) =
        if remaining = "" then
            List.rev acc
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

    if isStringified then
        // lex' 0 [] rawStr
        [ 1, Identifier "not implemented" ]
    else
        lex' 1 [] rawStr.[1..]
