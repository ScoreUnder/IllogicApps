module IllogicApps.Json.SerialisationHelper

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

let hackyInsertDecimalPoint (str: string) =
    if
        str.Contains('.', System.StringComparison.Ordinal)
        || str.Contains('e', System.StringComparison.OrdinalIgnoreCase)
    then
        str
    else
        $"{str}.0"
