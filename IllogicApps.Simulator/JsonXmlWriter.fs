namespace IllogicApps.Simulator

open System
open System.Collections.Generic
open System.Threading.Tasks
open System.Xml

open IllogicApps.Json

type private BuildingObject =
    | BuildingObject of MutableOrderedMap<string, List<BuildingObject>>
    | Text of string
    | Cooked of JsonTree

module private JsonXmlWriter =
    [<Literal>]
    let TEXT_KEY = "#text"

    [<Literal>]
    let CDATA_KEY = "#cdata-section"

    [<Literal>]
    let COMMENT_KEY = "#comment"

    [<Literal>]
    let DOCTYPE_KEY = "!DOCTYPE"

    [<Literal>]
    let DOCTYPE_NAME_KEY = "@name"

    [<Literal>]
    let DOCTYPE_PUBLIC_KEY = "@public"

    [<Literal>]
    let DOCTYPE_SYSTEM_KEY = "@system"

    [<Literal>]
    let DOCTYPE_SUBSET_KEY = "@internalSubset"

    let inline makeAttributeKey attrName = $"@{attrName}"
    let inline isAttributeKey (key: string) = key.StartsWith("@")
    let inline getAttributeName (key: string) = key.[1..]

open JsonXmlWriter

type JsonXmlWriter(bugForBugAccurate: bool) =
    inherit XmlWriter()

    let mutable writeState = WriteState.Start
    let stack = Stack<string * BuildingObject>()
    let mutable attributeKey: String option = None
    let mutable attributeValue: String option = None
    let mutable result: JsonTree option = None

    let raiseError msg =
        writeState <- WriteState.Error
        failwith msg

    let singletonList v =
        let list = List()
        list.Add(v)
        list

    let requireState states =
        if not (Array.contains writeState states) then
            raiseError $"Called in the wrong state: {writeState}"

    let requireStateNot states =
        if Array.contains writeState states then
            raiseError $"Called in the wrong state: {writeState}"

    let cookText =
        (function
        | Text t -> String t
        | _ -> raiseError "Expecting text under text key")

    let rec cook building =
        match building with
        | Cooked v -> v
        | Text s -> String s
        | BuildingObject obj ->
            if obj.Count = 0 then
                Null
            elif obj.Count = 1 && obj.ContainsKey(TEXT_KEY) then
                // Pure text node
                obj.[TEXT_KEY]
                |> Seq.map cookText
                |> List.ofSeq
                |> function
                    | [ String _ as s ] -> s
                    | l -> Conversions.createArray l
            else
                obj
                |> MutableOrderedMap.chooseValues (fun k ->
                    if k = COMMENT_KEY && bugForBugAccurate then
                        // Really silly special case for comments
                        function
                        | l when l.Count >= 2 -> Some Conversions.emptyArray
                        | _ -> None
                    else
                        function
                        | l when l.Count = 1 -> Some(cook l.[0])
                        | l -> Seq.map cook l |> Conversions.createArray |> Some)
                |> Seq.map (fun (KeyValue(k, v)) -> k, v)
                |> Conversions.createObject

    do stack.Push("", BuildingObject(MutableOrderedMap()))

    member private this.Current =
        match stack.Peek() with
        | _, BuildingObject map -> map
        | _ -> raiseError "Invalid state: not in an element"

    member public this.Result =
        match result with
        | Some r -> r
        | None -> raiseError "Not finished writing"

    override this.WriteState = writeState

    override this.Close() : unit =
        if stack.Count <> 1 && writeState <> WriteState.Error && result = None then
            raiseError "Not all elements have been closed"

        if writeState <> WriteState.Closed then
            this.Flush()
            writeState <- WriteState.Closed

    override this.Dispose(disposing: bool) : unit =
        if disposing then
            this.Close()

        base.Dispose(disposing)

    override this.DisposeAsyncCore() : ValueTask = base.DisposeAsyncCore()

    override this.Flush() : unit =
        match writeState with
        | WriteState.Start
        | WriteState.Prolog
        | WriteState.Error -> ()
        | WriteState.Closed -> raiseError "Already closed"
        | _ ->
            if stack.Count = 1 then
                let _, building = stack.Pop()
                let tree = cook building

                result <- Some tree

    override this.FlushAsync() : Task = raiseError "Not Implemented"

    override this.LookupPrefix(ns: string) : string =
        raiseError $"LookupPrefix({ns}): Note Implemented"

    override this.WriteAttributes(reader: XmlReader, defattr: bool) : unit = raiseError "Not Implemented"
    // override this.WriteAttributesAsync(reader: XmlReader, defattr: bool): Task =
    //     raiseError "Not Implemented"
    override this.WriteBase64(buffer: byte array, index: int, count: int) : unit = raiseError "Not Implemented"
    // override this.WriteBase64Async(buffer: byte array, index: int, count: int): Task =
    //     raiseError "Not Implemented"
    // override this.WriteBinHex(buffer: byte array, index: int, count: int): unit =
    //     raiseError "Not Implemented"
    // override this.WriteBinHexAsync(buffer: byte array, index: int, count: int): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteCData(text: string) : unit =
        requireState [| WriteState.Element; WriteState.Content |]

        this.Current.EnsureKey (fun _ -> List()) CDATA_KEY |> _.Add(Text text)
    // override this.WriteCDataAsync(text: string): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteCharEntity(ch: char) : unit = raiseError "Not Implemented"
    // override this.WriteCharEntityAsync(ch: char): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteChars(buffer: char array, index: int, count: int) : unit = raiseError "Not Implemented"
    // override this.WriteCharsAsync(buffer: char array, index: int, count: int): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteComment(text: string) : unit =
        requireState
            [| WriteState.Start
               WriteState.Prolog
               WriteState.Element
               WriteState.Content |]

        if writeState = WriteState.Element then
            writeState <- WriteState.Content

        this.Current.EnsureKey (fun _ -> List()) COMMENT_KEY |> _.Add(Text text)
    // override this.WriteCommentAsync(text: string): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteDocType(name: string, pubid: string, sysid: string, subset: string) : unit =
        if isNull name then
            raiseError "null name"

        if isNull subset then
            raiseError "null subset"

        requireState [| WriteState.Start; WriteState.Prolog |]

        this.WriteStartElement("", DOCTYPE_KEY, "")

        let map = this.Current

        if not (System.String.IsNullOrEmpty(name)) then
            map.[DOCTYPE_NAME_KEY] <- singletonList (Text name)

        if not (System.String.IsNullOrEmpty(pubid)) then
            map.[DOCTYPE_PUBLIC_KEY] <- singletonList (Text pubid)

        if not (System.String.IsNullOrEmpty(sysid)) then
            map.[DOCTYPE_SYSTEM_KEY] <- singletonList (Text sysid)

        if not (System.String.IsNullOrEmpty(subset)) then
            map.[DOCTYPE_SUBSET_KEY] <- singletonList (Text subset)

        this.WriteEndElement(true)

        writeState <- WriteState.Prolog
    // override this.WriteDocTypeAsync(name: string, pubid: string, sysid: string, subset: string): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteEndAttribute() : unit =
        requireState [| WriteState.Attribute |]

        let attributeName =
            match attributeKey with
            | Some k -> k
            | None -> raiseError "Not writing an attribute"

        let attributeValueStr =
            match attributeValue with
            | Some v -> v
            | None -> raiseError "Attribute value not set"

        this.Current.EnsureKey (fun _ -> List()) (makeAttributeKey attributeName)
        |> _.Add(Text attributeValueStr)

        attributeKey <- None
        attributeValue <- None

        writeState <- WriteState.Element
    // override this.WriteEndAttributeAsync(): System.Threading.Tasks.Task =
    // raiseError "Not Implemented"
    override this.WriteEndDocument() : unit = failwith "Not Implemented"
    // override this.WriteEndDocumentAsync(): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteEndElement() : unit = this.WriteEndElement(false)

    member this.WriteEndElement(full: bool) : unit =
        requireState [| WriteState.Element; WriteState.Attribute; WriteState.Content |]

        let name, building = stack.Pop()

        let tree =
            match cook building with
            | Null when full -> String ""
            | other -> other

        let ind = this.Current.[name].IndexOf(building)

        if ind = -1 then
            raiseError "Invalid state: missing building object from parent"

        this.Current.[name].[ind] <- Cooked tree

        writeState <- WriteState.Content
    // override this.WriteEndElementAsync(): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteEntityRef(name: string) : unit =
        raiseError $"WriteEntityRef({name}): Not Implemented"
    // override this.WriteEntityRefAsync(name: string): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteFullEndElement() : unit = this.WriteEndElement(true)
    // override this.WriteFullEndElementAsync(): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteName(name: string) : unit =
        raiseError $"WriteName({name}): Not Implemented"
    // override this.WriteNameAsync(name: string): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteNmToken(name: string) : unit =
        raiseError $"WriteNmToken({name}): Not Implemented"
    // override this.WriteNmTokenAsync(name: string): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteNode(reader: XmlReader, defattr: bool) : unit =
        raiseError $"WriteNode({reader}, {defattr}): Not Implemented"

    override this.WriteNode(navigator: XPath.XPathNavigator, defattr: bool) : unit =
        raiseError $"WriteNode({navigator}, {defattr}): Not Implemented"
    // override this.WriteNodeAsync(reader: XmlReader, defattr: bool): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    // override this.WriteNodeAsync(navigator: XPath.XPathNavigator, defattr: bool): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteProcessingInstruction(name: string, text: string) : unit =
        requireState [| WriteState.Start |]

        if name <> "xml" then
            raiseError $"WriteProcessingInstruction({name}, {text}): Only 'xml' is supported"

        this.WriteStartElement("", $"?{name}", "")

        // Parse attributes from text (TODO: this is shit lol)
        text.Split(" ")
        |> Seq.map (fun attribute ->
            let parts = attribute.Split("=", 2)

            if parts.Length <> 2 then
                raiseError $"WriteProcessingInstruction({name}, {text}): Invalid attribute '{attribute}'"

            parts.[0], parts.[1].Trim('"'))
        |> Seq.iter (fun (key, value) -> this.Current.[makeAttributeKey key] <- singletonList (Text value))

        this.WriteEndElement(true)

        writeState <- WriteState.Prolog

    // override this.WriteProcessingInstructionAsync(name: string, text: string): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteQualifiedName(localName: string, ns: string) : unit =
        raiseError $"WriteQualifiedName({localName}, {ns}): Not Implemented"
    // override this.WriteQualifiedNameAsync(localName: string, ns: string): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteRaw(buffer: char array, index: int, count: int) : unit =
        raiseError $"WriteRaw({buffer}, {index}, {count}): Not Implemented"

    override this.WriteRaw(data: string) : unit =
        failwith $"WriteRaw({data}): Not Implemented"
    // override this.WriteRawAsync(buffer: char array, index: int, count: int): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    // override this.WriteRawAsync(data: string): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteStartAttribute(prefix: string, localName: string, ns: string) : unit =
        requireState [| WriteState.Element |]

        if attributeKey.IsSome then
            raiseError "Already writing an attribute"

        if isNull prefix then
            raiseError "null prefix"

        if isNull localName then
            raiseError "null attribute name"

        requireState [| WriteState.Element |]

        attributeKey <- Some(if prefix = "" then localName else $"{prefix}:{localName}")
        writeState <- WriteState.Attribute

    // override this.WriteStartAttributeAsync(prefix: string, localName: string, ns: string): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteStartDocument() : unit =
        raiseError "WriteStartDocument(): Not Implemented"

    override this.WriteStartDocument(standalone: bool) : unit =
        raiseError $"WriteStartDocument({standalone}): Not Implemented"
    // override this.WriteStartDocumentAsync(): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    // override this.WriteStartDocumentAsync(standalone: bool): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteStartElement(prefix: string, localName: string, ns: string) : unit =
        requireState
            [| WriteState.Start
               WriteState.Prolog
               WriteState.Element
               WriteState.Attribute
               WriteState.Content |]

        if isNull prefix then
            raiseError "null prefix"

        let name = if prefix = "" then localName else $"{prefix}:{localName}"

        let elem = BuildingObject(MutableOrderedMap())
        this.Current.EnsureKey (fun _ -> List()) name |> _.Add(elem)
        stack.Push((name, elem))

        writeState <- WriteState.Element

    // override this.WriteStartElementAsync(prefix: string, localName: string, ns: string): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteString(text: string) : unit =
        requireState [| WriteState.Element; WriteState.Attribute; WriteState.Content |]

        if isNull text then
            raiseError "null text"

        if writeState = WriteState.Attribute then
            match attributeValue with
            | None -> attributeValue <- Some text
            | Some _ -> raiseError "Already written to this attribute"
        else
            this.Current.EnsureKey (fun _ -> List()) TEXT_KEY |> _.Add(Text text)
            writeState <- WriteState.Content

    // override this.WriteStringAsync(text: string): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    override this.WriteSurrogateCharEntity(lowChar: char, highChar: char) : unit =
        raiseError $"WriteSurrogateCharEntity({lowChar}, {highChar}): Not Implemented"
    // override this.WriteSurrogateCharEntityAsync(lowChar: char, highChar: char): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"
    // override this.WriteValue(value: bool): unit =
    //     raiseError "Not Implemented"
    // override this.WriteValue(value: System.DateTime): unit =
    //     raiseError "Not Implemented"
    // override this.WriteValue(value: System.DateTimeOffset): unit =
    //     raiseError "Not Implemented"
    // override this.WriteValue(value: decimal): unit =
    //     raiseError "Not Implemented"
    // override this.WriteValue(value: float): unit =
    //     raiseError "Not Implemented"
    // override this.WriteValue(value: int): unit =
    //     raiseError "Not Implemented"
    // override this.WriteValue(value: int64): unit =
    //     raiseError "Not Implemented"
    // override this.WriteValue(value: obj): unit =
    //     raiseError "Not Implemented"
    // override this.WriteValue(value: float32): unit =
    //     raiseError "Not Implemented"
    // override this.WriteValue(value: string): unit =
    //     raiseError "Not Implemented"
    override this.WriteWhitespace(ws: string) : unit =
        raiseError $"WriteWhitespace({ws}): Not Implemented"
    // override this.WriteWhitespaceAsync(ws: string): System.Threading.Tasks.Task =
    //     raiseError "Not Implemented"

module JsonToXmlConversion =
    type private ConversionState =
        | Element of string * JsonTree
        | EndElement

    let writeJsonToXml bugForBugAccurate (json: JsonTree) (xmlDoc: XmlDocument) =
        let getStringOrDefault k def (o: OrderedMap<string, JsonTree>) =
            OrderedMap.tryFind k o
            |> Option.map Conversions.rawStringOfJson
            |> Option.defaultValue def

        let sanitizeElementName name =
            String.collect
                (fun c ->
                    if XmlConvert.IsNCNameChar c || c = ':' then
                        new string (Array.singleton c)
                    else
                        let hex = (uint16 c).ToString("X4") in $"_x{hex}_")
                name

        let rec aux (xml: XmlNode) acc =
            match acc with
            | [] -> ()
            | [ Element(_, Object o) ] -> // root elem
                o
                |> OrderedMap.fold
                    (fun acc k v ->
                        match k, v with
                        | "?xml", Object o ->
                            let xmlDeclaration =
                                xmlDoc.CreateXmlDeclaration(
                                    o |> getStringOrDefault (makeAttributeKey "version") "1.0",
                                    o |> getStringOrDefault (makeAttributeKey "encoding") null,
                                    o |> getStringOrDefault (makeAttributeKey "standalone") null
                                )

                            xmlDoc.AppendChild(xmlDeclaration) |> ignore
                            acc
                        | "?xml", _ -> failwith "Invalid XML declaration"
                        | DOCTYPE_KEY, Object o ->
                            let doctype =
                                xmlDoc.CreateDocumentType(
                                    o |> getStringOrDefault DOCTYPE_NAME_KEY "",
                                    o |> getStringOrDefault DOCTYPE_PUBLIC_KEY null,
                                    o |> getStringOrDefault DOCTYPE_SYSTEM_KEY null,
                                    o |> getStringOrDefault DOCTYPE_SUBSET_KEY null
                                )

                            xmlDoc.AppendChild(doctype) |> ignore
                            acc
                        | DOCTYPE_KEY, _ -> failwith "Invalid DOCTYPE declaration"
                        | _ -> Element(k, v) :: acc)
                    []
                |> function
                    | [] -> ()
                    | [ el ] -> aux xml [ el; EndElement ]
                    | _ -> failwith "More than one root element specified"
            | Element(COMMENT_KEY, String text) :: rest ->
                if bugForBugAccurate then
                    aux xml (Element(sanitizeElementName COMMENT_KEY, String text) :: rest)
                else
                    let comment = xmlDoc.CreateComment(text)
                    xml.AppendChild(comment) |> ignore
                    aux xml rest
            | Element(CDATA_KEY, String text) :: rest ->
                let cdata = xmlDoc.CreateCDataSection(text)
                xml.AppendChild(cdata) |> ignore
                aux xml rest
            | Element(TEXT_KEY, String text) :: rest ->
                let textNode = xmlDoc.CreateTextNode(text)
                xml.AppendChild(textNode) |> ignore
                aux xml rest
            | Element(s, Object o) :: rest ->
                let attributes, elements = OrderedMap.partition (fun k _ -> isAttributeKey k) o

                let elem = xmlDoc.CreateElement(sanitizeElementName s)
                xml.AppendChild(elem) |> ignore

                attributes
                |> OrderedMap.iter (fun k v -> elem.SetAttribute(getAttributeName k, Conversions.ensureString v))

                let next =
                    OrderedMap.foldBack
                        (fun k v acc ->
                            match v with
                            | Array a -> (Seq.foldBack (fun v acc -> Element(k, v) :: acc) a acc)
                            | _ -> Element(k, v) :: acc)
                        elements
                        (EndElement :: rest)

                aux elem next
            | Element(s, Null) :: rest ->
                let elem = xmlDoc.CreateElement(sanitizeElementName s)
                xml.AppendChild(elem) |> ignore
                aux xml rest
            | Element(s, json) :: rest ->
                let text = Conversions.rawStringOfJson json
                let elem = xmlDoc.CreateElement(sanitizeElementName s)
                let textNode = xmlDoc.CreateTextNode(text)
                elem.AppendChild(textNode) |> ignore
                xml.AppendChild(elem) |> ignore
                aux xml rest
            | EndElement :: rest -> aux xml.ParentNode rest

        aux xmlDoc [ Element("", json) ]
