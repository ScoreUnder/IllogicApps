module IllogicApps.Json.Conversions

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
        | Object o when Map.isEmpty o -> "{}" :: acc
        | Object o -> "{" :: List.tail (Map.foldBack (fun k v acc -> "," :: "\"" :: k :: "\":" :: aux acc v) o ("}" :: acc))
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
