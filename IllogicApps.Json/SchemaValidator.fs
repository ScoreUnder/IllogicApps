module IllogicApps.Json.SchemaValidator

type StringComparison = System.StringComparison
type SortedSet<'T> = System.Collections.Generic.SortedSet<'T>
type IComparer<'T> = System.Collections.Generic.IComparer<'T>

open IllogicApps.Json.Conversions

// https://json-schema.org/draft/2020-12/json-schema-core

// 4.2.1 Instance Data Model
[<RequireQualifiedAccess>]
type SchemaType =
    | Null
    | Boolean
    | Object
    | Array
    | Number
    | Integer
    | String

let schemaTypeOfString =
    function
    | "null" -> SchemaType.Null
    | "boolean" -> SchemaType.Boolean
    | "object" -> SchemaType.Object
    | "array" -> SchemaType.Array
    | "number" -> SchemaType.Number
    | "integer" -> SchemaType.Integer
    | "string" -> SchemaType.String
    | v -> failwithf "Unknown schema type: %s" v

// 4.2.2 Instance Equality
let rec compareJsons (a: JsonTree) (b: JsonTree) : int =
    match a, b with
    | JsonTree.Null, JsonTree.Null -> 0
    | JsonTree.Boolean a, JsonTree.Boolean b -> compare a b
    | JsonTree.String a, JsonTree.String b -> compare a b
    | NumbersAsInteger(a, b) -> compare a b
    | NumbersAsFloat(a, b) -> compare a b
    | NumbersAsDecimal(a, b) -> compare a b
    | JsonTree.Array a, JsonTree.Array b ->
        let lengthCompare = compare a.Length b.Length

        if lengthCompare <> 0 then
            lengthCompare
        else
            Seq.map2 compareJsons a b
            |> Seq.tryFind (fun x -> x <> 0)
            |> Option.defaultValue 0
    | JsonTree.Object a, JsonTree.Object b ->
        let countCompare = compare a.Count b.Count

        if countCompare <> 0 then
            countCompare
        else
            let keysCompare =
                Seq.map2 compare (Seq.sort a.Keys) (Seq.sort b.Keys)
                |> Seq.tryFind (fun x -> x <> 0)
                |> Option.defaultValue 0

            if keysCompare <> 0 then
                keysCompare
            else
                Seq.tryPick (fun k1 -> let c = compareJsons a.[k1] b.[k1] in if c <> 0 then Some c else None) a.Keys
                |> Option.defaultValue 0
    | _ -> compare (JsonTree.getType a) (JsonTree.getType b)

let jsonsEqual (a: JsonTree) (b: JsonTree) = compareJsons a b = 0

type JsonSchema =
    { type_: SchemaType list option

      // Subschema combinators
      not: JsonSchema option
      allOf: JsonSchema list option
      anyOf: JsonSchema list option
      oneOf: JsonSchema list option
      ``if``: JsonSchema option
      ``then``: JsonSchema option
      ``else``: JsonSchema option

      // Generic schemas
      enum: JsonTree list option
      ``const``: JsonTree option

      // Embedded data schemas
      // TODO! https://json-schema.org/understanding-json-schema/reference/non_json_data
      // contentMediaType: string option
      // contentEncoding: string option
      // contentSchema: JsonSchema option

      // String schemas
      minLength: int option
      maxLength: int option
      pattern: string option

      // Number schemas
      multipleOf: decimal option
      minimum: decimal option
      exclusiveMinimum: decimal option
      maximum: decimal option
      exclusiveMaximum: decimal option

      // Array schemas
      items: JsonSchema option
      prefixItems: JsonSchema list option
      unevaluatedItems: JsonSchema option
      contains: JsonSchema option
      minContains: int option
      maxContains: int option
      minItems: int option
      maxItems: int option
      uniqueItems: bool option

      // Object schemas
      properties: OrderedMap<string, JsonSchema> option
      patternProperties: OrderedMap<string, JsonSchema> option
      additionalProperties: JsonSchema option
      unevaluatedProperties: JsonSchema option
      required: string list option
      propertyNames: JsonSchema option
      minProperties: int option
      maxProperties: int option
      dependentRequired: OrderedMap<string, string list> option
      dependentSchemas: OrderedMap<string, JsonSchema> option

      // Defs and refs
      ``$defs``: OrderedMap<string, JsonSchema> option
      ``$ref``: string option }

let emptyJsonSchema =
    { type_ = None

      // Subschema combinators
      not = None
      allOf = None
      anyOf = None
      oneOf = None
      ``if`` = None
      ``then`` = None
      ``else`` = None

      // Generic schemas
      enum = None
      ``const`` = None

      // String schemas
      minLength = None
      maxLength = None
      pattern = None

      // Number schemas
      multipleOf = None
      minimum = None
      exclusiveMinimum = None
      maximum = None
      exclusiveMaximum = None

      // Array schemas
      items = None
      prefixItems = None
      unevaluatedItems = None
      contains = None
      minContains = None
      maxContains = None
      minItems = None
      maxItems = None
      uniqueItems = None

      // Object schemas
      properties = None
      patternProperties = None
      additionalProperties = None
      unevaluatedProperties = None
      required = None
      propertyNames = None
      minProperties = None
      maxProperties = None
      dependentRequired = None
      dependentSchemas = None

      // Defs and refs
      ``$defs`` = None
      ``$ref`` = None }

let private mapArrayOrSingle f =
    function
    | Array a -> a |> Seq.map f |> List.ofSeq
    | v -> [ f v ]

let rec jsonSchemaOfJson json =
    let readOptionalSubSchemas key json =
        JsonTree.tryGetKey key json
        |> Option.map (ensureArray >> Seq.map jsonSchemaOfJson >> List.ofSeq)

    let readOptionalSubSchema key json =
        JsonTree.tryGetKey key json |> Option.map jsonSchemaOfJson

    let readOptionalSubSchemaMap key json =
        JsonTree.tryGetKey key json
        |> Option.map (ensureObject >> OrderedMap.mapValuesOnly jsonSchemaOfJson)

    let readOptionalStringList key json =
        JsonTree.tryGetKey key json
        |> Option.map (ensureArray >> Seq.map ensureString >> List.ofSeq)

    let readOptionalInt key json =
        JsonTree.tryGetKey key json |> Option.map (fun i -> ensureInteger i |> int)

    match json with
    | Boolean true -> emptyJsonSchema
    | Boolean false ->
        { emptyJsonSchema with
            not = Some emptyJsonSchema }
    | Object _ ->
        { type_ =
            JsonTree.tryGetKey "type" json
            |> Option.map (mapArrayOrSingle (ensureString >> schemaTypeOfString))
          not = readOptionalSubSchema "not" json
          allOf = readOptionalSubSchemas "allOf" json
          anyOf = readOptionalSubSchemas "anyOf" json
          oneOf = readOptionalSubSchemas "oneOf" json
          ``if`` = readOptionalSubSchema "if" json
          ``then`` = readOptionalSubSchema "then" json
          ``else`` = readOptionalSubSchema "else" json
          enum = JsonTree.tryGetKey "enum" json |> Option.map (ensureArray >> List.ofSeq)
          ``const`` = JsonTree.tryGetKey "const" json
          minLength = readOptionalInt "minLength" json
          maxLength = readOptionalInt "maxLength" json
          pattern = JsonTree.tryGetKey "pattern" json |> Option.map ensureString
          multipleOf = JsonTree.tryGetKey "multipleOf" json |> Option.map numberAsDecimal
          minimum = JsonTree.tryGetKey "minimum" json |> Option.map numberAsDecimal
          exclusiveMinimum = JsonTree.tryGetKey "exclusiveMinimum" json |> Option.map numberAsDecimal
          maximum = JsonTree.tryGetKey "maximum" json |> Option.map numberAsDecimal
          exclusiveMaximum = JsonTree.tryGetKey "exclusiveMaximum" json |> Option.map numberAsDecimal
          items = readOptionalSubSchema "items" json
          prefixItems = readOptionalSubSchemas "prefixItems" json
          unevaluatedItems = readOptionalSubSchema "unevaluatedItems" json
          contains = readOptionalSubSchema "contains" json
          minContains = readOptionalInt "minContains" json
          maxContains = readOptionalInt "maxContains" json
          minItems = readOptionalInt "minItems" json
          maxItems = readOptionalInt "maxItems" json
          uniqueItems = JsonTree.tryGetKey "uniqueItems" json |> Option.map ensureBoolean
          properties = readOptionalSubSchemaMap "properties" json
          patternProperties = readOptionalSubSchemaMap "patternProperties" json
          additionalProperties = readOptionalSubSchema "additionalProperties" json
          unevaluatedProperties = readOptionalSubSchema "unevaluatedProperties" json
          required = readOptionalStringList "required" json
          propertyNames = readOptionalSubSchema "propertyNames" json
          minProperties = readOptionalInt "minProperties" json
          maxProperties = readOptionalInt "maxProperties" json
          dependentRequired =
            JsonTree.tryGetKey "dependentRequired" json
            |> Option.map (
                ensureObject
                >> OrderedMap.mapValuesOnly (ensureArray >> Seq.map ensureString >> List.ofSeq)
            )
          dependentSchemas = readOptionalSubSchemaMap "dependentSchemas" json
          ``$defs`` =
            JsonTree.tryGetKey "$defs" json
            |> Option.map (ensureObject >> OrderedMap.mapValuesOnly jsonSchemaOfJson)
          ``$ref`` = JsonTree.tryGetKey "$ref" json |> Option.map ensureString }
    | _ -> failwith "Invalid JSON schema"

let typesMatch (schemaType: SchemaType) (json: JsonTree) =
    match schemaType, json with
    | SchemaType.Null, Null -> true
    | SchemaType.Boolean, Boolean _ -> true
    | SchemaType.Object, Object _ -> true
    | SchemaType.Array, Array _ -> true
    | SchemaType.Number, Integer _ -> true
    | SchemaType.Number, Float _ -> true
    | SchemaType.Number, Decimal _ -> true
    | SchemaType.Integer, Integer _ -> true
    | SchemaType.Integer, Float f when System.Double.IsInteger(f) -> true
    | SchemaType.Integer, Decimal d when System.Decimal.Truncate(d) = d -> true
    | SchemaType.String, String _ -> true
    | _ -> false

let inline private seqCount ([<InlineIfLambda>] f) seq =
    Seq.sumBy (fun x -> if f x then 1 else 0) seq

let private listOfDistinct (cmpf: 'a -> 'a -> int) (seq: 'a seq) =
    let comparer =
        { new IComparer<'a> with
            member this.Compare(x, y) = cmpf x y }

    let seenSet = SortedSet<'a>(comparer)

    Seq.fold (fun acc x -> if seenSet.Add x then x :: acc else acc) [] seq
    |> List.rev

type 't JsonSchemaSingleResult =
    | Ok of 't
    | Error of string
    | NotImplemented of string
    | Warning of string

module JsonSchemaSingleResult =
    let map f =
        function
        | Ok x -> Ok(f x)
        | Error e -> Error e
        | NotImplemented e -> NotImplemented e
        | Warning w -> Warning w

    let bind f =
        function
        | Ok x -> f x
        | Error e -> Error e
        | NotImplemented e -> NotImplemented e
        | Warning w -> Warning w

    let mapError f =
        function
        | Ok x -> Ok x
        | Error e -> Error(f e)
        | NotImplemented e -> NotImplemented e
        | Warning w -> Warning w

    let bindError f =
        function
        | Ok x -> Ok x
        | Error e -> f e
        | NotImplemented e -> NotImplemented e
        | Warning w -> Warning w

    let exists f =
        function
        | Ok x -> f x
        | Error _ -> false
        | NotImplemented _ -> false
        | Warning _ -> false

type JsonSchemaResultMessage =
    { schemaPath: string
      jsonPath: string
      result: Unit JsonSchemaSingleResult }

type JsonSchemaResult =
    { messages: JsonSchemaResultMessage list
      isMatch: bool }

module JsonSchemaResult =
    let empty = { messages = []; isMatch = true }

    let merge a b =
        { messages = a.messages @ b.messages
          isMatch = a.isMatch && b.isMatch }

    let add schemaPath jsonPath single result =
        match single with
        | Error v ->
            { messages =
                { schemaPath = schemaPath
                  jsonPath = jsonPath
                  result = Error v }
                :: result.messages
              isMatch = false }
        | NotImplemented v ->
            { messages =
                { schemaPath = schemaPath
                  jsonPath = jsonPath
                  result = NotImplemented v }
                :: result.messages
              isMatch = result.isMatch }
        | Warning v ->
            { messages =
                { schemaPath = schemaPath
                  jsonPath = jsonPath
                  result = Warning v }
                :: result.messages
              isMatch = result.isMatch }
        | Ok _ -> result

    let addOpt schemaPath jsonPath single result =
        match single with
        | Some v -> add schemaPath jsonPath v result
        | None -> result

    let mergeMany results result = List.fold merge result results

    let formatMessages messages =
        messages
        |> List.map (fun m ->
            match m.result with
            | Ok _ -> ""
            | NotImplemented v -> $"Not implemented: {m.schemaPath} {m.jsonPath} {v}"
            | Warning v -> $"Warning: {m.schemaPath} {m.jsonPath} {v}"
            | Error v -> $"Error: {m.schemaPath} {m.jsonPath} {v}")
        |> String.concat "\n"

type JsonSchemaResultData =
    { result: JsonSchemaResult
      matchedItemsDeep: int Set
      matchedPropertiesDeep: string Set }

module JsonSchemaResultData =
    let empty =
        { result = JsonSchemaResult.empty
          matchedItemsDeep = Set.empty
          matchedPropertiesDeep = Set.empty }

    let merge a b =
        if a.result.isMatch then
            { result = JsonSchemaResult.merge a.result b.result
              matchedItemsDeep = max a.matchedItemsDeep b.matchedItemsDeep
              matchedPropertiesDeep = Set.union a.matchedPropertiesDeep b.matchedPropertiesDeep }
        else
            { b with
                result = JsonSchemaResult.merge a.result b.result }

    let add schemaPath jsonPath single result =
        { result with
            JsonSchemaResultData.result = JsonSchemaResult.add schemaPath jsonPath single result.result }

    let addOpt schemaPath jsonPath single result =
        { result with
            JsonSchemaResultData.result = JsonSchemaResult.addOpt schemaPath jsonPath single result.result }

    let mergeOpt maybeResult result =
        match maybeResult with
        | Some result2 -> merge result2 result
        | None -> result

    let mergeMany results result = Seq.fold merge result results

    let createFailedFromSingle schemaPath jsonPath value =
        { empty with
            result =
                { messages =
                    [ { schemaPath = schemaPath
                        jsonPath = jsonPath
                        result = value |> JsonSchemaSingleResult.map (fun _ -> ()) } ]
                  isMatch = false } }

    let extractFromSingle schemaPath jsonPath =
        function
        | Ok v -> v
        | message -> createFailedFromSingle schemaPath jsonPath message

let resolveRef (schema: JsonSchema) (refName: string) =
    if refName = "#" then
        Ok schema
    else if not (refName.StartsWith("#/")) then
        Warning $"Invalid $ref: {refName}"
    else if not (refName.StartsWith("#/$defs/")) then
        NotImplemented $"Unsupported $ref: {refName}"
    else
        match schema.``$defs`` with
        | Some defs ->
            let refName = refName.[8..] in

            defs
            |> OrderedMap.findMapOrElse refName Ok (fun () -> Warning $"$ref {refName} not found")
        | None -> Warning "Reference used, but no $defs block found"

let private validateType schemaTypes json =
    if List.exists (fun schemaType -> typesMatch schemaType json) schemaTypes then
        Ok()
    else
        Error $"Type mismatch: expected {schemaTypes}"

let validateJsonSchema (rootSchema: JsonSchema) (rootJson: JsonTree) : JsonSchemaResult =
    let rec aux isInsideRef schemaPath jsonPath (schema: JsonSchema) (json: JsonTree) : JsonSchemaResultData =
        let validateAnyOf schemaPath jsonPath schemas json =
            let rec validateAnyOf' acc ind schemas json =
                match schemas with
                | [] ->
                    JsonSchemaResultData.mergeMany
                        acc
                        (JsonSchemaResultData.createFailedFromSingle schemaPath jsonPath (Error "No match"))
                | h :: t ->
                    let result = aux isInsideRef $"{schemaPath}/{ind}" jsonPath h json

                    if result.result.isMatch then
                        result
                    else
                        validateAnyOf' (result :: acc) (ind + 1) t json

            match schemas with
            | Some schemas -> validateAnyOf' [] 0 schemas json
            | None -> JsonSchemaResultData.empty

        let validateOneOf schemaPath jsonPath schemas json =
            let rec validateOneOf' fails okays ind schemas json =
                match schemas with
                | [] ->
                    match okays with
                    | [] ->
                        JsonSchemaResultData.mergeMany
                            fails
                            (JsonSchemaResultData.createFailedFromSingle schemaPath jsonPath (Error "No match"))
                    | [ single ] -> single
                    | _ ->
                        (JsonSchemaResultData.createFailedFromSingle schemaPath jsonPath (Error "More than one match"))
                | h :: t ->
                    let result = aux isInsideRef $"{schemaPath}/{ind}" jsonPath h json

                    if result.result.isMatch then
                        validateOneOf' fails (result :: okays) (ind + 1) t json
                    else
                        validateOneOf' (result :: fails) okays (ind + 1) t json

            match schemas with
            | Some schemas -> validateOneOf' [] [] 0 schemas json
            | None -> JsonSchemaResultData.empty

        let validateSimple f message =
            function
            | None -> Ok()
            | Some v when f v -> Ok()
            | _ -> Error message

        let result =
            JsonSchemaResultData.empty
            |> JsonSchemaResultData.addOpt
                $"{schemaPath}/type"
                jsonPath
                (schema.type_ |> Option.map (fun t -> validateType t json))
            |> JsonSchemaResultData.addOpt
                $"{schemaPath}/not"
                jsonPath
                (schema.not
                 |> Option.map (fun subSchema ->
                     let notResult = aux isInsideRef $"{schemaPath}/not" jsonPath subSchema json
                     System.Console.WriteLine(notResult)

                     if aux isInsideRef $"{schemaPath}/not" jsonPath subSchema json |> _.result.isMatch then
                         Error "should not have validated, but did"
                     else
                         Ok()))
            |> JsonSchemaResultData.mergeMany (
                schema.allOf
                |> Option.defaultValue []
                |> List.mapi (fun i subSchema -> aux isInsideRef $"{schemaPath}/allOf/{i}" jsonPath subSchema json)
            )
            |> JsonSchemaResultData.merge (validateAnyOf $"{schemaPath}/anyOf" jsonPath schema.anyOf json)
            |> JsonSchemaResultData.merge (validateOneOf $"{schemaPath}/oneOf" jsonPath schema.oneOf json)
            |> JsonSchemaResultData.add
                $"{schemaPath}/enum"
                jsonPath
                (validateSimple (List.exists (jsonsEqual json)) "Enum value not correct" schema.enum)
            |> JsonSchemaResultData.add
                $"{schemaPath}/const"
                jsonPath
                (validateSimple (jsonsEqual json) "Const value not correct" schema.``const``)
            |> JsonSchemaResultData.mergeOpt (
                schema.``if``
                |> Option.bind (fun ifSchema ->
                    if aux isInsideRef $"{schemaPath}/if" jsonPath ifSchema json |> _.result.isMatch then
                        schema.``then``
                        |> Option.map (fun subSchema -> aux isInsideRef $"{schemaPath}/then" jsonPath subSchema json)
                    else
                        schema.``else``
                        |> Option.map (fun subSchema -> aux isInsideRef $"{schemaPath}/else" jsonPath subSchema json))
            )
            |> JsonSchemaResultData.mergeOpt (
                schema.``$ref``
                |> Option.map (fun refName ->
                    if isInsideRef then
                        Warning "Not allowed to nest refs (infinite recursion possible)"
                    else
                        resolveRef rootSchema refName
                        |> JsonSchemaSingleResult.map (fun schema ->
                            aux true (refName.TrimStart('#')) jsonPath schema json)
                    |> JsonSchemaResultData.extractFromSingle $"{schemaPath}/$ref" jsonPath)
            )

        match json with
        | String s ->
            result
            |> JsonSchemaResultData.add
                $"{schemaPath}/minLength"
                jsonPath
                (validateSimple (fun minLength -> s.Length >= minLength) "String is too short" schema.minLength)
            |> JsonSchemaResultData.add
                $"{schemaPath}/maxLength"
                jsonPath
                (validateSimple (fun maxLength -> s.Length <= maxLength) "String is too long" schema.maxLength)
            |> JsonSchemaResultData.add
                $"{schemaPath}/pattern"
                jsonPath
                (validateSimple
                    (fun pattern -> System.Text.RegularExpressions.Regex.IsMatch(s, pattern))
                    "String pattern does not match"
                    schema.pattern)
        | Integer _
        | Float _
        | Decimal _ ->
            let number = numberAsDecimal json in

            result
            |> JsonSchemaResultData.add
                $"{schemaPath}/multipleOf"
                jsonPath
                (validateSimple
                    (fun multipleOf -> number % multipleOf = 0m)
                    "Number is not a multiple of multipleOf"
                    schema.multipleOf)
            |> JsonSchemaResultData.add
                $"{schemaPath}/minimum"
                jsonPath
                (validateSimple (fun minimum -> number >= minimum) "Number is less than minimum" schema.minimum)
            |> JsonSchemaResultData.add
                $"{schemaPath}/exclusiveMinimum"
                jsonPath
                (validateSimple
                    (fun exclusiveMinimum -> number > exclusiveMinimum)
                    "Number is not greater than exclusiveMinimum"
                    schema.exclusiveMinimum)
            |> JsonSchemaResultData.add
                $"{schemaPath}/maximum"
                jsonPath
                (validateSimple (fun maximum -> number <= maximum) "Number is greater than maximum" schema.maximum)
            |> JsonSchemaResultData.add
                $"{schemaPath}/exclusiveMaximum"
                jsonPath
                (validateSimple
                    (fun exclusiveMaximum -> number < exclusiveMaximum)
                    "Number is not less than exclusiveMaximum"
                    schema.exclusiveMaximum)
        | Array a ->
            let numPrefixItems =
                match schema.prefixItems with
                | None -> 0
                | Some prefixItems -> min (List.length prefixItems) a.Length in

            result
            |> JsonSchemaResultData.mergeOpt (
                schema.prefixItems
                |> Option.map (fun subSchemas ->
                    Seq.mapi2
                        (fun i schema json ->
                            { (aux false $"{schemaPath}/prefixItems/{i}" $"{jsonPath}/{i}" schema json) with
                                matchedItemsDeep = Set.singleton i })
                        subSchemas
                        a
                    |> Seq.fold JsonSchemaResultData.merge JsonSchemaResultData.empty)
            )
            |> JsonSchemaResultData.mergeOpt (
                schema.items
                |> Option.map (fun subSchema ->
                    a
                    |> Seq.skip numPrefixItems
                    |> Seq.mapi (fun i json ->
                        { (aux false $"{schemaPath}/items" $"{jsonPath}/{i + numPrefixItems}" subSchema json) with
                            matchedItemsDeep = Set.singleton i })
                    |> Seq.fold JsonSchemaResultData.merge JsonSchemaResultData.empty)
            )
            |> JsonSchemaResultData.mergeOpt (
                schema.contains
                |> Option.map (fun subSchema ->
                    let containsCount, containsResult =
                        a
                        |> Seq.mapi (fun i json ->
                            { (aux false $"{schemaPath}/contains" $"{jsonPath}/{i}" subSchema json) with
                                matchedItemsDeep = Set.singleton i })
                        |> Seq.fold
                            (fun (cnt, acc) result ->
                                cnt + (if result.result.isMatch then 1 else 0), JsonSchemaResultData.merge acc result)
                            (0, JsonSchemaResultData.empty)

                    let containsResult =
                        match schema.minContains with
                        | None when containsCount = 0 ->
                            JsonSchemaResultData.add
                                $"{schemaPath}/contains"
                                jsonPath
                                (Error "Array does not contain any items that match the schema")
                                containsResult
                        | Some minContains when containsCount < minContains ->
                            JsonSchemaResultData.add
                                $"{schemaPath}/minContains"
                                jsonPath
                                (Error "Array does not contain enough items that match the schema")
                                containsResult
                        | _ -> JsonSchemaResultData.empty // Clear any error messages; we've satisfied enough

                    containsResult
                    |> JsonSchemaResultData.add
                        $"{schemaPath}/maxContains"
                        jsonPath
                        (validateSimple
                            (fun maxContains -> containsCount <= maxContains)
                            "Array contains too many items that match the schema"
                            schema.maxContains))
            )
            |> (fun result ->
                JsonSchemaResultData.mergeOpt
                    (schema.unevaluatedItems
                     |> Option.map (fun subSchema ->
                         a
                         |> Seq.mapi (fun i json ->
                             if Set.contains i result.matchedItemsDeep then
                                 JsonSchemaResultData.empty
                             else
                                 { (aux false $"{schemaPath}/unevaluatedItems" $"{jsonPath}/{i}" subSchema json) with
                                     matchedItemsDeep = Set.empty })
                         |> Seq.fold JsonSchemaResultData.merge JsonSchemaResultData.empty))
                    result)
            |> JsonSchemaResultData.add
                $"{schemaPath}/minItems"
                jsonPath
                (validateSimple (fun minItems -> a.Length >= minItems) "Array is too short" schema.minItems)
            |> JsonSchemaResultData.add
                $"{schemaPath}/maxItems"
                jsonPath
                (validateSimple (fun maxItems -> a.Length <= maxItems) "Array is too long" schema.maxItems)
            |> JsonSchemaResultData.add
                $"{schemaPath}/uniqueItems"
                jsonPath
                (validateSimple
                    (fun uniqueItems ->
                        if uniqueItems then
                            a |> listOfDistinct compareJsons |> List.length = a.Length
                        else
                            true)
                    "Array contains duplicate items"
                    schema.uniqueItems)
        | Object o ->
            result
            |> JsonSchemaResultData.mergeOpt (
                schema.properties
                |> Option.map (fun properties ->
                    properties
                    |> Seq.map (fun (KeyValue(prop, subSchema)) ->
                        match OrderedMap.tryFind prop o with
                        | Some v ->
                            { (aux false $"{schemaPath}/properties/{prop}" $"{jsonPath}/{prop}" subSchema v) with
                                matchedPropertiesDeep = Set.singleton prop }
                        | None -> JsonSchemaResultData.empty)
                    |> Seq.fold JsonSchemaResultData.merge JsonSchemaResultData.empty)
            )
            |> JsonSchemaResultData.mergeOpt (
                schema.patternProperties
                |> Option.map (fun patternProperties ->
                    patternProperties
                    |> Seq.collect (fun (KeyValue(pattern, subSchema)) ->
                        o
                        |> Seq.filter (fun (KeyValue(prop, _)) ->
                            System.Text.RegularExpressions.Regex.IsMatch(prop, pattern))
                        |> Seq.map (fun (KeyValue(prop, v)) ->
                            { (aux false $"{schemaPath}/patternProperties/{pattern}" $"{jsonPath}/{prop}" subSchema v) with
                                matchedPropertiesDeep = Set.singleton prop }))
                    |> Seq.fold JsonSchemaResultData.merge JsonSchemaResultData.empty)
            )
            |> JsonSchemaResultData.mergeOpt (
                let propertiesSet =
                    match schema.properties with
                    | Some properties -> properties |> OrderedMap.keys |> Set.ofSeq
                    | None -> Set.empty

                schema.additionalProperties
                |> Option.map (fun subSchema ->
                    o
                    |> Seq.filter (fun (KeyValue(prop, _)) -> not (Set.contains prop propertiesSet))
                    |> Seq.map (fun (KeyValue(prop, v)) ->
                        { (aux false $"{schemaPath}/additionalProperties" $"{jsonPath}/{prop}" subSchema v) with
                            matchedPropertiesDeep = Set.singleton prop })
                    |> Seq.fold JsonSchemaResultData.merge JsonSchemaResultData.empty)
            )
            |> JsonSchemaResultData.mergeMany (
                schema.dependentSchemas
                |> Option.defaultValue OrderedMap.empty
                |> Seq.map (fun (KeyValue(k, subSchema)) ->
                    match OrderedMap.tryFind k o with
                    | Some v ->
                        // Note: `k` itself doesn't seem to count as an 'evaluated' property..?
                        aux isInsideRef $"{schemaPath}/dependentSchemas/{k}" jsonPath subSchema v
                    | None -> JsonSchemaResultData.empty)
            )
            |> JsonSchemaResultData.mergeOpt (
                schema.unevaluatedProperties
                |> Option.map (fun subSchema ->
                    o
                    |> Seq.filter (fun (KeyValue(prop, _)) -> not (Set.contains prop result.matchedPropertiesDeep))
                    |> Seq.map (fun (KeyValue(prop, v)) ->
                        { (aux false $"{schemaPath}/unevaluatedProperties" $"{jsonPath}/{prop}" subSchema v) with
                            matchedPropertiesDeep = Set.empty })
                    |> Seq.fold JsonSchemaResultData.merge JsonSchemaResultData.empty)
            )
            |> JsonSchemaResultData.add
                $"{schemaPath}/required"
                jsonPath
                (validateSimple (List.forall o.ContainsKey) "Object is missing required properties" schema.required)
            |> JsonSchemaResultData.mergeOpt (
                schema.propertyNames
                |> Option.map (fun subSchema ->
                    o
                    |> Seq.map (fun (KeyValue(prop, _)) ->
                        { (aux false $"{schemaPath}/propertyNames" $"{jsonPath}/{prop}" subSchema (String prop)) with
                            matchedPropertiesDeep = Set.empty })
                    |> Seq.fold JsonSchemaResultData.merge JsonSchemaResultData.empty)
            )
            |> JsonSchemaResultData.add
                $"{schemaPath}/minProperties"
                jsonPath
                (validateSimple (fun min -> o.Count >= min) "Object is too small" schema.minProperties)
            |> JsonSchemaResultData.add
                $"{schemaPath}/maxProperties"
                jsonPath
                (validateSimple (fun max -> o.Count <= max) "Object is too large" schema.maxProperties)
            |> JsonSchemaResultData.mergeMany (
                schema.dependentRequired
                |> Option.defaultValue OrderedMap.empty
                |> Seq.collect (fun (KeyValue(k, required)) ->
                    match OrderedMap.tryFind k o with
                    | Some _ ->
                        required
                        |> Seq.filter (fun prop -> not (o.ContainsKey prop))
                        |> Seq.map (fun prop ->
                            JsonSchemaResultData.createFailedFromSingle
                                $"{schemaPath}/dependentRequired/{k}"
                                $"{jsonPath}/{prop}"
                                (Error "Object is missing required properties"))
                    | None -> Seq.empty)
            )
        | _ -> result

    aux false "" "" rootSchema rootJson |> _.result
