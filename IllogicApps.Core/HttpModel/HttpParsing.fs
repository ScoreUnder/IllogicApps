module IllogicApps.Core.HttpModel.HttpParsing

open System
open System.IO
open System.Net
open System.Text
open IllogicApps.Core.Support
open IllogicApps.Json

let parseQueryString =
    function
    | null
    | ""
    | "?" -> OrderedMap.empty
    | query ->
        let query = if query.StartsWith("?") then query.Substring(1) else query

        query.Split('&')
        |> Seq.map (fun part ->
            let parts = part.Split('=', 2)

            match parts with
            | [| key; value |] -> WebUtility.UrlDecode(key), WebUtility.UrlDecode(value)
            | [| value |] -> null, WebUtility.UrlDecode(value)
            | _ -> failwith "Should never happen")
        |> Seq.groupBy fst
        |> Seq.map (fun (key, values) -> key, (values |> Seq.map snd |> String.concat ","))
        |> OrderedMap.ofSeq

let plainDecodeByContentType contentType (body: byte array) =
    let charset =
        ContentType.Charset.get contentType |> Option.defaultValue Encoding.UTF8

    use stream = new MemoryStream(body)
    use reader = new StreamReader(stream, charset)
    reader.ReadToEnd()

let decodeFormUrlEncoded (contentType: string) (body: byte array) =
    let decoded = plainDecodeByContentType contentType body

    let formData =
        decoded.Split('&')
        |> Seq.map (fun part ->
            let key, value =
                match part.Split('=', 2) with
                | [| key; value |] -> WebUtility.UrlDecode(key), WebUtility.UrlDecode(value)
                | [| key |] -> WebUtility.UrlDecode(key), ""
                | _ -> failwith "Should never happen"

            OrderedMap.Builder().Add("key", String key).Add("value", String value).Build()
            |> Object)
        |> Conversions.createArray

    Blob.ofBytes contentType body
    |> Conversions.ensureObject
    |> OrderedMap.setAtEnd "$formdata" formData
    |> Object

let private multipartEndMarker = Encoding.UTF8.GetBytes("--")
let private httpNewline = Encoding.UTF8.GetBytes("\r\n")
let private doubleHttpNewline = Encoding.UTF8.GetBytes("\r\n\r\n")

let rec decodeMultipartFormData (contentType: string) (body: byte array) : JsonTree =
    let boundary = ContentType.getMultipartBoundary contentType |> Option.get
    let boundaryBytes = Encoding.UTF8.GetBytes("--" + boundary)

    let mutable parts = []
    let mutable bodySpan = body.AsSpan()
    let mutable start = bodySpan.IndexOf(boundaryBytes)

    if start <> 0 then
        failwith "Invalid multipart/form-data body: no initial boundary"

    while start <> -1 do
        bodySpan <- bodySpan.Slice(start + boundaryBytes.Length)
        let next = bodySpan.IndexOf(boundaryBytes)

        if next = -1 then
            if
                bodySpan.Length >= 2
                && bodySpan
                    .TrimEnd([| byte ' '; byte '\r'; byte '\n'; byte '\t' |])
                    .SequenceEqual(multipartEndMarker.AsSpan())
            then
                start <- -1
            else
                failwith "Invalid multipart/form-data body: no final boundary"
        elif next < 2 then
            failwith "Invalid multipart/form-data body: consecutive boundaries"
        else
            if not (bodySpan.Slice(0, 2).SequenceEqual(httpNewline.AsSpan())) then
                failwith "Invalid multipart/form-data body: no newline after boundary"

            if not (bodySpan.Slice(next - 2, 2).SequenceEqual(httpNewline.AsSpan())) then
                failwith "Invalid multipart/form-data body: no newline before boundary"

            let part = bodySpan.Slice(2, next - 4)

            let headerEnd = part.IndexOf(doubleHttpNewline)

            if headerEnd = -1 then
                failwith "Invalid multipart/form-data body: no header/body separator"

            let mutable rawHeaders = part.Slice(0, headerEnd)
            let headers = OrderedMap.Builder()

            let mutable nextHeader = rawHeaders.IndexOf(httpNewline)

            while nextHeader <> -2 do
                let header =
                    if nextHeader = -1 then
                        nextHeader <- -2
                        rawHeaders
                    else
                        let header = rawHeaders.Slice(0, nextHeader)
                        rawHeaders <- rawHeaders.Slice(nextHeader + 2)
                        nextHeader <- rawHeaders.IndexOf(httpNewline)
                        header

                let colon = header.IndexOf(byte ':')

                if colon = -1 then
                    failwith "Invalid multipart/form-data body: no colon in header"

                let key = Encoding.UTF8.GetString(header.Slice(0, colon))

                let value =
                    Encoding.UTF8.GetString(header.Slice(colon + 1).TrimStart([| byte ' '; byte '\t' |]))

                headers.Add(key, String value) |> ignore

            let body = part.Slice(headerEnd + 4)

            let headers = headers.TryAdd("Content-Length", String(string body.Length)).Build()

            let contentType =
                headers
                |> OrderedMap.findCaseInsensitiveMapOrElse "Content-Type" Conversions.ensureString (fun () ->
                    ContentType.Binary)

            let part =
                OrderedMap
                    .Builder()
                    .Add("headers", headers |> Object)
                    .Add("body", decodeBodyByContentType contentType (body.ToArray()))
                    .Build()
                |> Object

            parts <- part :: parts
            start <- next

    Blob.ofBytes contentType body
    |> Conversions.ensureObject
    |> OrderedMap.setAtEnd "$multipart" (parts |> List.rev |> Conversions.createArray)
    |> Object

and decodeBodyByContentType (contentType: string) (body: byte array) =
    let decodeBody () =
        plainDecodeByContentType contentType body

    if ContentType.isJson contentType then
        JsonParser.parse (decodeBody ())
    elif ContentType.isAnyText contentType then
        String(decodeBody ())
    elif ContentType.isMultipartFormData contentType then
        decodeMultipartFormData contentType body
    elif ContentType.isFormUrlEncoded contentType then
        decodeFormUrlEncoded contentType body
    else
        Blob.ofBytes contentType body

let decodeOptionalBodyByContentType (contentType: string option) (body: byte array) =
    if body.Length = 0 && Option.isNone contentType then
        None
    else
        let contentType = Option.defaultValue ContentType.Binary contentType
        Some(decodeBodyByContentType contentType body)

let httpStatusCodeIsSuccess (code: int) = code >= 200 && code < 300
