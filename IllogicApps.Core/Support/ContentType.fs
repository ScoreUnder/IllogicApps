module IllogicApps.Core.Support.ContentType

open System
open System.Text

[<Literal>]
let Binary = "application/octet-stream"

[<Literal>]
let Xml = "application/xml"

[<Literal>]
let Json = "application/json"

[<Literal>]
let Text = "text/plain"

[<Literal>]
let MultipartFormData = "multipart/form-data"

[<Literal>]
let FormUrlEncoded = "application/x-www-form-urlencoded"

// Content-Types with charsets
[<Literal>]
let XmlUtf8 = Xml + ";charset=utf-8"

[<Literal>]
let JsonUtf8 = Json + ";charset=utf-8"

[<Literal>]
let TextUtf8 = Text + ";charset=utf-8"

let mimePart (contentType: string) =
    match contentType.IndexOf(';') with
    | -1 -> contentType.AsSpan()
    | semicolon -> contentType.AsSpan(0, semicolon)

let isBinary (contentType: string) =
    (mimePart contentType).Equals(Binary, StringComparison.OrdinalIgnoreCase)

let isXml (contentType: string) =
    (mimePart contentType).Equals(Xml, StringComparison.OrdinalIgnoreCase)

let isJson (contentType: string) =
    (mimePart contentType).Equals(Json, StringComparison.OrdinalIgnoreCase)

let isAnyText (contentType: string) =
    contentType.StartsWith("text/", StringComparison.OrdinalIgnoreCase)

let isMultipartFormData (contentType: string) =
    (mimePart contentType)
        .Equals(MultipartFormData, StringComparison.OrdinalIgnoreCase)

let isFormUrlEncoded (contentType: string) =
    (mimePart contentType)
        .Equals(FormUrlEncoded, StringComparison.OrdinalIgnoreCase)

let getMultipartBoundary (contentType: string) =
    contentType.Split(';', StringSplitOptions.TrimEntries)
    |> Array.tryPick (fun v ->
        let prefix = "boundary="

        if v.StartsWith(prefix, StringComparison.OrdinalIgnoreCase) then
            Some(v.Substring(prefix.Length))
        else
            None)

module Charset =
    [<Literal>]
    let private CharsetEq = "charset="

    [<Literal>]
    let Utf8 = "utf-8"

    let set (contentType: string) (charset: string) = $"{contentType};{CharsetEq}{charset}"

    let get (contentType: string) =
        contentType.Split(';', StringSplitOptions.TrimEntries)
        |> Array.tryPick (fun v ->
            if v.StartsWith(CharsetEq, StringComparison.OrdinalIgnoreCase) then
                Some(Encoding.GetEncoding(v.Substring(CharsetEq.Length)))
            else
                None)

    let getBuggy (contentType: string) =
        let parts = contentType.Split(';', 3)

        match parts with
        | [| _; second; _ |]
        | [| _; second |] when second.StartsWith(CharsetEq, StringComparison.OrdinalIgnoreCase) ->
            Some(Encoding.GetEncoding(second.Substring CharsetEq.Length))
        | _ -> None

    let decodeBytes (contentType: string) (content: byte array) =
        contentType.Split(";")
        |> Seq.skip 1
        |> Seq.iter (fun seg ->
            if not (seg.Contains('=', StringComparison.Ordinal)) then
                failwithf "Bad content type segment %s" seg)

        get contentType |> Option.defaultValue Encoding.UTF8 |> _.GetString(content)
