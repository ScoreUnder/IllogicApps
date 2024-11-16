// NOTICE: This file is a translation of equivalent files in the LogicAppUnit project, and as such is licensed
// under the same terms as that project. The license can be found in this project's directory.
namespace LogicAppUnit.Helper

open System.Net.Http.Headers
open System.Security.Cryptography.Xml
open System
open System.IO
open System.Net.Http
open System.Reflection
open System.Runtime.InteropServices
open System.Text
open System.Xml

open Newtonsoft.Json
open Newtonsoft.Json.Linq

open LogicAppUnit

type ContentHelper =
    static let JsonContentType = "application/json"
    static let PlainTextContentType = "text/plain"
    static let XmlContentType = "application/xml"

    // {{{ Stream Content
    static member CreateStreamContent(stream: Stream, contentType: string) : StreamContent =
        ArgumentNullException.ThrowIfNull(stream, nameof stream)
        ArgumentNullException.ThrowIfNull(contentType, nameof contentType)

        let content = new StreamContent(stream)
        content.Headers.ContentType <- MediaTypeHeaderValue(contentType)
        content

    static member CreateJsonStreamContent(jsonStream: Stream) : StreamContent =
        ContentHelper.CreateStreamContent(jsonStream, JsonContentType)

    static member CreatePlainStreamContent(stream: Stream) : StreamContent =
        ContentHelper.CreateStreamContent(stream, PlainTextContentType)
    // }}}

    // {{{ String Content
    static member CreateStringContent
        (value: string, contentType: string, [<Optional; DefaultParameterValue(null: Encoding)>] encoding: Encoding) : StringContent =
        ArgumentNullException.ThrowIfNull(value, nameof value)
        ArgumentNullException.ThrowIfNull(contentType, nameof contentType)

        let encoding = if encoding = null then Encoding.UTF8 else encoding

        new StringContent(value, encoding, contentType)

    static member CreateJsonStringContent(jsonString: string) : StringContent =
        new StringContent(jsonString, Encoding.UTF8, JsonContentType)

    static member CreatePlainStringContent(value: string) : StringContent =
        new StringContent(value, Encoding.UTF8, PlainTextContentType)

    static member CreateXmlStringContent(xmlString: string) : StringContent =
        new StringContent(xmlString, Encoding.UTF8, XmlContentType)
    // }}}

    static member CreateJsonStringContent(jsonObject: obj) : StringContent =
        ArgumentNullException.ThrowIfNull(jsonObject, nameof jsonObject)

        let json = JsonConvert.SerializeObject(jsonObject)
        new StringContent(json, Encoding.UTF8, JsonContentType)

    static member CreateXmlStringContent(xmlDoc: XmlDocument) : StringContent =
        ArgumentNullException.ThrowIfNull(xmlDoc, nameof xmlDoc)

        new StringContent(xmlDoc.ToString(), Encoding.UTF8, XmlContentType)

    // {{{ Stream Conversion
    static member ConvertStreamToString(input: Stream) : string =
        ArgumentNullException.ThrowIfNull(input, nameof input)

        use reader = new StreamReader(input)
        reader.ReadToEnd()

    static member ConvertStringToStream(input: string) : Stream =
        ArgumentNullException.ThrowIfNull(input, nameof input)

        let byteArray = Encoding.ASCII.GetBytes input
        new MemoryStream(byteArray)
    // }}}

    // {{{ Formatting
    static member FormatJson(json: string) : string =
        let json =
            json
                .Replace(Environment.MachineName, "localhost")
                .Replace(Environment.MachineName.ToLowerInvariant(), "localhost")

        let settings = JsonLoadSettings(CommentHandling = CommentHandling.Ignore)
        JObject.Parse(json, settings).ToString()

    static member FormatXml(xmlStream: Stream) : string =
        ArgumentNullException.ThrowIfNull(xmlStream, nameof xmlStream)

        let xmlDoc = XmlDocument()
        xmlDoc.Load(xmlStream)

        ContentHelper.FormatXml(xmlDoc)

    static member FormatXml(xml: string) : string =
        ArgumentNullException.ThrowIfNull(xml, nameof xml)

        let xmlDoc = XmlDocument()
        xmlDoc.LoadXml(xml)

        ContentHelper.FormatXml(xmlDoc)

    static member FormatXml(xmlDoc: XmlDocument) : string =
        ArgumentNullException.ThrowIfNull(xmlDoc, nameof xmlDoc)

        let xmlTransform = XmlDsigC14NTransform()
        xmlTransform.LoadInput(xmlDoc)

        let xmlCanonical = xmlTransform.GetOutput(typeof<Stream>) :?> Stream
        ContentHelper.ConvertStreamToString(xmlCanonical)
// }}}

type ResourceHelper =
    static member GetAssemblyResourceAsStream(resourceName: string) : Stream =
        ResourceHelper.GetAssemblyResourceAsStream(resourceName, Assembly.GetExecutingAssembly())

    static member GetAssemblyResourceAsStream(resourceName: string, containingAssembly: Assembly) : Stream =
        ArgumentNullException.ThrowIfNull(resourceName, nameof resourceName)
        ArgumentNullException.ThrowIfNull(containingAssembly, nameof containingAssembly)

        let resourceData = containingAssembly.GetManifestResourceStream(resourceName)

        if resourceData = null then
            raise
            <| TestException(
                $"The resource '{resourceName}' could not be found in assembly '{containingAssembly.GetName().Name}'. Make sure that the resource name is a fully qualified name (including the .NET namespace), that the correct assembly is referenced and the resource is built as an Embedded Resource."
            )

        resourceData

    static member GetAssemblyResourceAsString(resourceName: string) : string =
        ContentHelper.ConvertStreamToString(
            ResourceHelper.GetAssemblyResourceAsStream(resourceName, Assembly.GetExecutingAssembly())
        )

    static member GetAssemblyResourceAsString(resourceName: string, containingAssembly: Assembly) : string =
        ContentHelper.ConvertStreamToString(
            ResourceHelper.GetAssemblyResourceAsStream(resourceName, containingAssembly)
        )
