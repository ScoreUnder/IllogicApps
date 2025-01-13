module IllogicApps.Json.SerialisationHelper

open System

let escapeStringForJson (str: string) =
    let rec aux ind =
        if ind >= str.Length then
            -1
        else
            match str.[ind] with
            | '\\'
            | '\"'
            | '\b'
            | '\f'
            | '\n'
            | '\r'
            | '\t' -> ind
            | c when c < char 0x20 -> ind
            | _ -> aux (ind + 1)

    match aux 0 with
    | -1 -> str // Avoid creating a new string if we can get away with it
    | ind ->
        let sb = System.Text.StringBuilder(2 * String.length str)
        let span = str.AsSpan()
        sb.Append(span.Slice(0, ind)) |> ignore

        for c in span.Slice(ind) do
            match c with
            | '\\' -> sb.Append("\\\\")
            | '\"' -> sb.Append("\\\"")
            | '\b' -> sb.Append("\\b")
            | '\f' -> sb.Append("\\f")
            | '\n' -> sb.Append("\\n")
            | '\r' -> sb.Append("\\r")
            | '\t' -> sb.Append("\\t")
            | _ when c < char 0x20 -> sb.AppendFormat("\\u{0:x4}", Convert.ToInt32(c))
            | _ -> sb.Append(c)
            |> ignore

        sb.ToString()

let hackyInsertDecimalPoint (str: string) =
    if
        str.Contains('.', StringComparison.Ordinal)
        || str.Contains('e', StringComparison.OrdinalIgnoreCase)
    then
        str
    else
        $"{str}.0"
