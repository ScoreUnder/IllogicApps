namespace IllogicApps.Core.Support

open System
open System.Text
open IllogicApps.Json

[<Measure>]
type Base64

module Base64 =
    let inline mark (str: string<'m>) = MeasuredString.mark<Base64, 'm> str

    let inline unmark (str: string<Base64 * 'm>) = MeasuredString.unmark<Base64, 'm> str

    let inline ofBytes bytes =
        bytes |> MeasuredString.ofByteArray Convert.ToBase64String |> mark

    let inline toBytes str =
        str |> unmark |> MeasuredString.toByteArray Convert.FromBase64String

    let ofString str =
        str |> MeasuredString.toByteArray (fun v -> Encoding.UTF8.GetBytes v) |> ofBytes

    let toString str =
        str
        |> toBytes
        |> MeasuredString.ofByteArray (fun v -> Encoding.UTF8.GetString v)

module Blob =
    let ofBase64 (contentType: string) (base64: string<Base64>) : JsonTree =
        Conversions.createObject
            [ "$content-type", String contentType
              "$content", String(Base64.unmark base64) ]

    let ofString (contentType: string) (str: string) : JsonTree =
        str |> Base64.ofString |> ofBase64 contentType

    let ofBytes (contentType: string) (bytes: byte array) : JsonTree =
        bytes |> Base64.ofBytes |> ofBase64 contentType

    let binaryOfString (str: string) : JsonTree = str |> ofString ContentType.Binary

    let (|Blob|_|) (node: JsonTree) : (string * byte array) option =
        match node with
        | Object node ->
            if node.ContainsKey("$content-type") && node.ContainsKey("$content") then
                let content =
                    node["$content"] |> Conversions.rawStringOfJson |> Base64.mark |> Base64.toBytes

                Some(Conversions.rawStringOfJson node["$content-type"], content)
            else
                None
        | _ -> None
