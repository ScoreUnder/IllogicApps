namespace IllogicApps.Core.Support

open System
open System.Text
open IllogicApps.Json

[<Measure>]
type Base64

module Base64 =
    let inline mark (str: string<'m>) = MeasuredString.mark<Base64, 'm> str

    let inline unmark (str: string<Base64 * 'm>) = MeasuredString.unmark<Base64, 'm> str

    let inline ofBytes (bytes: byte[]) = Convert.ToBase64String bytes |> mark

    let inline toBytes (str: string<Base64>) =
        str |> unmark |> Convert.FromBase64String

    let ofString (str: string) =
        str |> Encoding.UTF8.GetBytes |> ofBytes

    let toString (str: string<Base64>) : string =
        str |> toBytes |> Encoding.UTF8.GetString

module Blob =
    let ofBase64 (contentType: string) (base64: string<Base64>) : JsonTree =
        Conversions.createObject
            [ "$content-type", String contentType
              "$content", String(Base64.unmark base64) ]

    let ofString (contentType: string) (str: string) : JsonTree =
        str |> Base64.ofString |> ofBase64 contentType

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
