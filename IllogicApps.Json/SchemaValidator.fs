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

type JsonSchemaExec =
    // Subschema combinators
    | Not of JsonSubSchema
    | AllOf of JsonSubSchema list
    | AnyOf of JsonSubSchema list
    | OneOf of JsonSubSchema list
    | IfThenElse of JsonSubSchema * JsonSubSchema * JsonSubSchema

    // Generic schemas
    | Invalid
    | TypeTest of SchemaType list
    | Enum of JsonTree list
    | Const of JsonTree

    // Embedded data schemas
    // TODO! https://json-schema.org/understanding-json-schema/reference/non_json_data
    // | ContentMediaType of string
    // | ContentEncoding of string
    // | ContentSchema of JsonSchema

    // String schemas
    | MinLength of int
    | MaxLength of int
    | Pattern of string

    // Number schemas
    | MultipleOf of decimal
    | Minimum of decimal
    | ExclusiveMinimum of decimal
    | Maximum of decimal
    | ExclusiveMaximum of decimal

    // Array schemas
    | Items of JsonSubSchema
    | PrefixItems of JsonSubSchema list
    | PrefixItemsAll of JsonSubSchema
    | UnevaluatedItems of JsonSubSchema
    | Contains of JsonSubSchema
    | MinContains of int
    | MaxContains of int
    | MinItems of int
    | MaxItems of int
    | UniqueItems

    // Object schemas
    | Properties of OrderedMap<string, JsonSubSchema>
    | PatternProperties of OrderedMap<string, JsonSubSchema>
    | AdditionalProperties of JsonSubSchema
    | UnevaluatedProperties of JsonSubSchema
    | Required of string list
    | PropertyNames of JsonSubSchema
    | MinProperties of int
    | MaxProperties of int
    | DependentRequired of OrderedMap<string, string list>
    | DependentSchemas of OrderedMap<string, JsonSubSchema>

    // Defs and refs
    | Ref of string

and JsonSubSchema = (string * JsonSchemaExec) list

and JsonSchema =
    { schema: JsonSubSchema
      subSchemas: Map<string, JsonSubSchema> }

let trueJsonSubSchema: JsonSubSchema = []
let falseJsonSubSchema schemaPath : JsonSubSchema = [ schemaPath, Invalid ]
let emptyJsonSchema: JsonSchema = { schema = []; subSchemas = Map.empty }

let private mapArrayOrSingle f =
    function
    | Array a -> a |> Seq.map f |> List.ofSeq
    | v -> [ f v ]

type private SubSchemaParseState =
    { ifCond: JsonSubSchema option
      thenBlock: JsonSubSchema option
      elseBlock: JsonSubSchema option }

/// <summary>Parse a <see cref="JsonSubSchema"/> from raw JSON.</summary>
/// <returns>the parsed sub-schema and a set of all referenced sub-schemas</returns>
let rec subSchemaOfJson (schemaPath: string) (json: JsonTree) : JsonSubSchema * string Set =
    match json with
    | Boolean true -> trueJsonSubSchema, Set.empty
    | Boolean false -> falseJsonSubSchema schemaPath, Set.empty
    | Object o ->
        o
        |> OrderedMap.fold
            (fun
                ((execFirst: JsonSubSchema,
                  execMid: JsonSubSchema,
                  execLast: JsonSubSchema,
                  refs: string Set,
                  state: SubSchemaParseState) as acc)
                k
                v ->
                match k with
                | "type" ->
                    let types = v |> mapArrayOrSingle (ensureString >> schemaTypeOfString)
                    ($"{schemaPath}/{k}", TypeTest types) :: execFirst, execMid, execLast, refs, state
                | "not"
                | "propertyNames" ->
                    let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}" v

                    let exec =
                        match k with
                        | "not" -> Not subSchema
                        | "propertyNames" -> PropertyNames subSchema
                        | _ -> failwith "Internal error: missing case"

                    ($"{schemaPath}/{k}", exec) :: execFirst, execMid, execLast, Set.union refs2 refs, state
                | "unevaluatedItems"
                | "unevaluatedProperties" ->
                    let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}" v

                    let exec =
                        match k with
                        | "unevaluatedItems" -> UnevaluatedItems subSchema
                        | "unevaluatedProperties" -> UnevaluatedProperties subSchema
                        | _ -> failwith "Internal error: missing case"

                    execFirst, execMid, ($"{schemaPath}/{k}", exec) :: execLast, Set.union refs2 refs, state
                | "items"
                | "contains"
                | "additionalProperties" ->
                    let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}" v

                    let exec =
                        match k with
                        | "items" -> Items subSchema
                        | "contains" -> Contains subSchema
                        | "additionalProperties" -> AdditionalProperties subSchema
                        | _ -> failwith "Internal error: missing case"

                    execFirst, ($"{schemaPath}/{k}", exec) :: execMid, execLast, Set.union refs2 refs, state
                | "anyOf"
                | "allOf"
                | "oneOf" ->
                    let _, subSchemas, refs2 =
                        v
                        |> ensureArray
                        |> Seq.fold
                            (fun (i, subSchemas, refs) x ->
                                let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}/{i}" x
                                i + 1, subSchema :: subSchemas, Set.union refs2 refs)
                            (0, [], Set.empty)

                    let exec =
                        match k with
                        | "anyOf" -> AnyOf subSchemas
                        | "allOf" -> AllOf subSchemas
                        | "oneOf" -> OneOf subSchemas
                        | _ -> failwith "Internal error: missing case"

                    ($"{schemaPath}/{k}", exec) :: execFirst, execMid, execLast, Set.union refs2 refs, state
                | "prefixItems" ->
                    // Differs slightly from the above case in that we can have a single
                    // schema instead of an array, in which case it is applied to all
                    // items in the array.
                    match v with
                    | Array a ->
                        let _, subSchemas, refs2 =
                            a
                            |> Seq.fold
                                (fun (i, subSchemas, refs) x ->
                                    let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}/{i}" x
                                    i + 1, subSchema :: subSchemas, Set.union refs2 refs)
                                (0, [], Set.empty)

                        let execFirst = ($"{schemaPath}/{k}", PrefixItems(List.rev subSchemas)) :: execFirst
                        execFirst, execMid, execLast, Set.union refs2 refs, state
                    | _ ->
                        let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}" v

                        let execFirst = ($"{schemaPath}/{k}", PrefixItemsAll subSchema) :: execFirst
                        execFirst, execMid, execLast, Set.union refs2 refs, state
                | "if" ->
                    let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}" v
                    let state = { state with ifCond = Some subSchema }
                    execFirst, execMid, execLast, Set.union refs2 refs, state
                | "then" ->
                    let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}" v

                    let state =
                        { state with
                            thenBlock = Some subSchema }

                    execFirst, execMid, execLast, Set.union refs2 refs, state
                | "else" ->
                    let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}" v

                    let state =
                        { state with
                            elseBlock = Some subSchema }

                    execFirst, execMid, execLast, Set.union refs2 refs, state
                | "properties"
                | "patternProperties"
                | "dependentSchemas" ->
                    let subSchemaMap, refs2 =
                        v
                        |> ensureObject
                        |> OrderedMap.fold
                            (fun (builder: OrderedMap.Builder<string, JsonSubSchema>, refs) prop sub ->
                                let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}/{prop}" sub
                                builder.Add(prop, subSchema), Set.union refs2 refs)
                            (OrderedMap.Builder(), Set.empty)

                    let subSchemaMap = subSchemaMap.Build()

                    let exec =
                        match k with
                        | "properties" -> Properties subSchemaMap
                        | "patternProperties" -> PatternProperties subSchemaMap
                        | "dependentSchemas" -> DependentSchemas subSchemaMap
                        | _ -> failwith "Internal error: missing case"

                    ($"{schemaPath}/{k}", exec) :: execFirst, execMid, execLast, Set.union refs2 refs, state
                | "enum" ->
                    let enumValues = v |> ensureArray |> List.ofSeq
                    ($"{schemaPath}/{k}", Enum enumValues) :: execFirst, execMid, execLast, refs, state
                | "const" -> ($"{schemaPath}/{k}", Const v) :: execFirst, execMid, execLast, refs, state
                | "minLength"
                | "maxLength"
                | "minContains"
                | "maxContains"
                | "minItems"
                | "maxItems"
                | "minProperties"
                | "maxProperties" ->
                    let value = v |> ensureInteger |> int

                    let exec =
                        match k with
                        | "minLength" -> MinLength value
                        | "maxLength" -> MaxLength value
                        | "minContains" -> MinContains value
                        | "maxContains" -> MaxContains value
                        | "minItems" -> MinItems value
                        | "maxItems" -> MaxItems value
                        | "minProperties" -> MinProperties value
                        | "maxProperties" -> MaxProperties value
                        | _ -> failwith "Internal error: missing case"

                    ($"{schemaPath}/{k}", exec) :: execFirst, execMid, execLast, refs, state
                | "pattern" ->
                    let value = v |> ensureString
                    execFirst, ($"{schemaPath}/{k}", Pattern value) :: execMid, execLast, refs, state
                | "multipleOf"
                | "minimum"
                | "exclusiveMinimum"
                | "maximum"
                | "exclusiveMaximum" ->
                    let value = v |> numberAsDecimal

                    let exec =
                        match k with
                        | "multipleOf" -> MultipleOf value
                        | "minimum" -> Minimum value
                        | "exclusiveMinimum" -> ExclusiveMinimum value
                        | "maximum" -> Maximum value
                        | "exclusiveMaximum" -> ExclusiveMaximum value
                        | _ -> failwith "Internal error: missing case"

                    execFirst, ($"{schemaPath}/{k}", exec) :: execMid, execLast, refs, state
                | "uniqueItems" ->
                    let value = v |> ensureBoolean

                    if value then
                        execFirst, ($"{schemaPath}/{k}", UniqueItems) :: execMid, execLast, refs, state
                    else
                        acc
                | "required" ->
                    let requiredProperties = v |> ensureArray |> Seq.map ensureString |> List.ofSeq
                    execFirst, ($"{schemaPath}/{k}", Required requiredProperties) :: execMid, execLast, refs, state
                | "dependentRequired" ->
                    let dependentRequired =
                        v
                        |> ensureObject
                        |> OrderedMap.mapValuesOnly (fun v -> v |> ensureArray |> Seq.map ensureString |> List.ofSeq)

                    let execMid = ($"{schemaPath}/{k}", DependentRequired dependentRequired) :: execMid
                    execFirst, execMid, execLast, refs, state
                | "$ref" ->
                    let ref = v |> ensureString
                    ($"{schemaPath}/{k}", Ref ref) :: execFirst, execMid, execLast, Set.add ref refs, state
                | _ ->
                    // Unknown key
                    // ...warning or something?
                    acc)
            ([],
             [],
             [],
             Set.empty,
             { ifCond = None
               thenBlock = None
               elseBlock = None })
        |> fun (execFirst, execMid, execLast, refs, state) ->
            let ifBlock =
                match state.ifCond with
                | None -> []
                | Some cond ->
                    [ schemaPath,
                      IfThenElse(
                          cond,
                          state.thenBlock |> Option.defaultValue trueJsonSubSchema,
                          state.elseBlock |> Option.defaultValue trueJsonSubSchema
                      ) ]

            ifBlock @ List.rev execFirst @ List.rev execMid @ List.rev execLast, refs

    | _ ->
        failwith
            $"Invalid JSON schema: Expected an object or boolean at '{schemaPath}', but got {JsonTree.getType json} instead."

/// <summary>Parse a <see cref="JsonSchema"/> from raw JSON.</summary>
let jsonSchemaOfJson json =
    let parseFromRef (ref: string) : JsonSubSchema * string Set =
        if ref = "#" then
            failwith "Internal error: Should have removed #-ref"
        else if ref = "" then
            failwithf "$ref cannot be empty"
        else
            let ref = ref.[1..]
            let refPath = ref.Split('/')

            if refPath.[0] <> "" then
                failwith $"Invalid $ref: #{ref}"

            refPath
            |> Seq.skip 1
            |> Seq.fold
                (fun acc elem ->
                    match acc with
                    | Array a ->
                        let ind = int elem in

                        if ind < 0 || ind >= a.Length then
                            failwith $"Nonexistent $ref (index {ind} out of bounds): #{ref}"
                        else
                            a.[ind]
                    | Object o ->
                        match OrderedMap.tryFind elem o with
                        | None -> failwith $"Nonexistent $ref (key '{elem}' not found in object): #{ref}"
                        | Some json -> json
                    | _ -> failwith $"Nonexistent $ref (lookup on non-container type): #{ref}")
                json
            |> subSchemaOfJson ref

    let rec resolveRefs (refs: string Set) (known: Map<string, JsonSubSchema>) =
        let allRefs = Set.difference refs (known.Keys |> Set.ofSeq) |> Set.remove "#"

        if Set.isEmpty allRefs then
            known
        else
            let nextSchemas =
                allRefs |> Seq.map (fun name -> name, parseFromRef name) |> Seq.toArray

            let nextRefs = nextSchemas |> Seq.map (fun (_, (_, refs)) -> refs) |> Set.unionMany

            let nextKnown =
                nextSchemas
                |> Seq.fold (fun acc (name, (schema, _)) -> Map.add name schema acc) known

            resolveRefs nextRefs nextKnown

    let initialSubSchema, initialRefs = subSchemaOfJson "" json

    { schema = initialSubSchema
      subSchemas = resolveRefs initialRefs Map.empty }

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

let private countDistinct (cmpf: 'a -> 'a -> int) (seq: 'a seq) =
    let comparer =
        { new IComparer<'a> with
            member this.Compare(x, y) = cmpf x y }

    let seenSet = SortedSet<'a>(comparer)

    Seq.iter (fun x -> seenSet.Add x |> ignore) seq
    seenSet.Count

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
      prefixLength: int
      minContains: int
      maxContains: int option
      matchedPropertiesDeep: string Set
      matchedProperties: string Set }

module JsonSchemaResultData =
    let empty =
        { result = JsonSchemaResult.empty
          matchedItemsDeep = Set.empty
          prefixLength = 0
          minContains = 1
          maxContains = None
          matchedPropertiesDeep = Set.empty
          matchedProperties = Set.empty }

    let merge newResult origResult =
        if newResult.result.isMatch then
            { origResult with
                result = JsonSchemaResult.merge newResult.result origResult.result
                matchedItemsDeep = max newResult.matchedItemsDeep origResult.matchedItemsDeep
                matchedPropertiesDeep = Set.union newResult.matchedPropertiesDeep origResult.matchedPropertiesDeep }
        else
            { origResult with
                result = JsonSchemaResult.merge newResult.result origResult.result }

    let add schemaPath jsonPath single result =
        { result with
            JsonSchemaResultData.result = JsonSchemaResult.add schemaPath jsonPath single result.result }

    let mergeMany results origResult =
        Seq.fold (fun acc el -> merge el acc) origResult results

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
        Ok schema.schema
    else
        match Map.tryFind refName schema.subSchemas with
        | Some subSchema -> Ok subSchema
        | None -> Warning $"Reference {refName} not found in schema"

let inline private validateSimple2 ([<InlineIfLambda>] f: Unit -> bool) message = if f () then Ok() else Error message

let private validateSimple f message =
    function
    | None -> Ok()
    | Some v when f v -> Ok()
    | _ -> Error message

let validateJsonSchema (rootSchema: JsonSchema) (rootJson: JsonTree) : JsonSchemaResult =
    let rec processSingle isInsideRef jsonPath json acc (schemaPath, schemaExec) =
        let addOne result =
            JsonSchemaResultData.add schemaPath jsonPath result acc

        let addFull result = JsonSchemaResultData.merge result acc

        let addMany results =
            JsonSchemaResultData.mergeMany results acc

        match schemaExec with
        | Not subSchema ->
            validateSimple2
                (fun () -> not (validateSubSchema isInsideRef jsonPath subSchema json).result.isMatch)
                "should not have validated, but did"
            |> addOne
        | AllOf subSchemas ->
            subSchemas
            |> List.map (fun subSchema -> validateSubSchema isInsideRef jsonPath subSchema json)
            |> addMany
        | AnyOf subSchemas ->
            let rec validateAnyOf' acc ind schemas json =
                match schemas with
                | [] ->
                    JsonSchemaResultData.mergeMany
                        acc
                        (JsonSchemaResultData.createFailedFromSingle schemaPath jsonPath (Error "No match"))
                | h :: t ->
                    let result = validateSubSchema isInsideRef jsonPath h json

                    if result.result.isMatch then
                        result
                    else
                        validateAnyOf' (result :: acc) (ind + 1) t json

            validateAnyOf' [] 0 subSchemas json |> addFull
        | OneOf subSchemas ->
            subSchemas
            |> List.map (fun subSchema -> validateSubSchema isInsideRef jsonPath subSchema json)
            |> List.partition (_.result.isMatch)
            |> function
                | [], fails ->
                    JsonSchemaResultData.mergeMany
                        fails
                        (JsonSchemaResultData.createFailedFromSingle schemaPath jsonPath (Error "No match"))
                | [ single ], _ -> single
                | _, _ -> JsonSchemaResultData.createFailedFromSingle schemaPath jsonPath (Error "More than one match")
            |> addFull
        | Enum values ->
            validateSimple2 (fun () -> List.exists (jsonsEqual json) values) "Enum value not correct"
            |> addOne
        | Const value ->
            validateSimple2 (fun () -> jsonsEqual json value) "Const value not correct"
            |> addOne
        | TypeTest types ->
            validateSimple2
                (fun () -> List.exists (fun t -> typesMatch t json) types)
                $"Type mismatch: expected {types}"
            |> addOne
        | IfThenElse(cond, thenBlock, elseBlock) ->
            if (validateSubSchema isInsideRef jsonPath cond json).result.isMatch then
                validateSubSchema isInsideRef jsonPath thenBlock json
            else
                validateSubSchema isInsideRef jsonPath elseBlock json
            |> addFull
        | Ref refName ->
            if isInsideRef then
                Warning "Not allowed to nest refs (infinite recursion possible)"
            else
                resolveRef rootSchema refName
                |> JsonSchemaSingleResult.map (fun schema -> validateSubSchema true jsonPath schema json)
            |> JsonSchemaResultData.extractFromSingle schemaPath jsonPath
            |> addFull
        | MinLength minLength ->
            match json with
            | String s ->
                validateSimple2 (fun () -> s.Length >= minLength) "String is too short"
                |> addOne
            | _ -> acc
        | MaxLength maxLength ->
            match json with
            | String s -> validateSimple2 (fun () -> s.Length <= maxLength) "String is too long" |> addOne
            | _ -> acc
        | Pattern pattern ->
            match json with
            | String s ->
                validateSimple2
                    (fun () -> System.Text.RegularExpressions.Regex.IsMatch(s, pattern))
                    "String pattern does not match"
                |> addOne
            | _ -> acc
        | MultipleOf multipleOf ->
            match json with
            | Integer _
            | Float _
            | Decimal _ ->
                let number = numberAsDecimal json in

                validateSimple2 (fun () -> number % multipleOf = 0m) "Number is not a multiple of multipleOf"
                |> addOne
            | _ -> acc
        | Minimum minimum ->
            match json with
            | Integer _
            | Float _
            | Decimal _ ->
                let number = numberAsDecimal json in

                validateSimple2 (fun () -> number >= minimum) "Number is less than minimum"
                |> addOne
            | _ -> acc
        | ExclusiveMinimum exclusiveMinimum ->
            match json with
            | Integer _
            | Float _
            | Decimal _ ->
                let number = numberAsDecimal json in

                validateSimple2 (fun () -> number > exclusiveMinimum) "Number is not greater than exclusiveMinimum"
                |> addOne
            | _ -> acc
        | Maximum maximum ->
            match json with
            | Integer _
            | Float _
            | Decimal _ ->
                let number = numberAsDecimal json in

                validateSimple2 (fun () -> number <= maximum) "Number is greater than maximum"
                |> addOne
            | _ -> acc
        | ExclusiveMaximum exclusiveMaximum ->
            match json with
            | Integer _
            | Float _
            | Decimal _ ->
                let number = numberAsDecimal json in

                validateSimple2 (fun () -> number < exclusiveMaximum) "Number is not less than exclusiveMaximum"
                |> addOne
            | _ -> acc
        | PrefixItems subSchemas ->
            match json with
            | Array a ->
                Seq.mapi2
                    (fun i schema json ->
                        { (validateSubSchema false $"{jsonPath}/{i}" schema json) with
                            matchedItemsDeep = Set.singleton i })
                    subSchemas
                    a
                |> addMany
                |> fun result ->
                    { result with
                        prefixLength = List.length subSchemas }
            | _ -> acc
        | PrefixItemsAll subSchema ->
            match json with
            | Array a ->
                Seq.mapi
                    (fun i json ->
                        { (validateSubSchema false $"{jsonPath}/{i}" subSchema json) with
                            matchedItemsDeep = Set.singleton i })
                    a
                |> addMany
            | _ -> acc
        | Items subSchema ->
            let numPrefixItems = acc.prefixLength

            match json with
            | Array a when a.Length > numPrefixItems ->
                a
                |> Seq.skip acc.prefixLength
                |> Seq.mapi (fun i json ->
                    { (validateSubSchema false $"{jsonPath}/{i + numPrefixItems}" subSchema json) with
                        matchedItemsDeep = Set.singleton (i + numPrefixItems) })
                |> addMany
            | _ -> acc
        | Contains subSchema ->
            match json with
            | Array a ->
                let containsCount, containsResult =
                    a
                    |> Seq.mapi (fun i json ->
                        { (validateSubSchema false $"{jsonPath}/{i}" subSchema json) with
                            matchedItemsDeep = Set.singleton i })
                    |> Seq.fold
                        (fun (cnt, acc) result ->
                            cnt + (if result.result.isMatch then 1 else 0), JsonSchemaResultData.merge acc result)
                        (0, JsonSchemaResultData.empty)

                let containsResult =
                    if containsCount < acc.minContains then
                        JsonSchemaResultData.add
                            schemaPath
                            jsonPath
                            (Error(
                                if containsCount = 0 then
                                    "Array does not contain any items that match the schema"
                                else
                                    "Array does not contain enough items that match the schema"
                            ))
                            containsResult
                    else
                        // Clear any error messages; we've satisfied enough
                        JsonSchemaResultData.empty

                containsResult
                |> JsonSchemaResultData.add
                    schemaPath
                    jsonPath
                    (validateSimple
                        (fun maxContains -> containsCount <= maxContains)
                        "Array contains too many items that match the schema"
                        acc.maxContains)
                |> addFull
            | _ -> acc
        | MinContains minContains -> { acc with minContains = minContains }
        | MaxContains maxContains ->
            { acc with
                maxContains = Some maxContains }
        | UnevaluatedItems subSchema ->
            match json with
            | Array a ->
                a
                |> Seq.mapi (fun i json ->
                    if Set.contains i acc.matchedItemsDeep then
                        JsonSchemaResultData.empty
                    else
                        { (validateSubSchema false $"{jsonPath}/{i}" subSchema json) with
                            matchedItemsDeep = Set.empty })
                |> addMany
            | _ -> acc
        | MinItems minItems ->
            match json with
            | Array a -> validateSimple2 (fun () -> a.Length >= minItems) "Array is too short" |> addOne
            | _ -> acc
        | MaxItems maxItems ->
            match json with
            | Array a -> validateSimple2 (fun () -> a.Length <= maxItems) "Array is too long" |> addOne
            | _ -> acc
        | UniqueItems ->
            match json with
            | Array a ->
                validateSimple2 (fun () -> countDistinct compareJsons a = a.Length) "Array contains duplicate items"
                |> addOne
            | _ -> acc
        | Properties properties ->
            match json with
            | Object o ->
                properties
                |> Seq.map (fun (KeyValue(prop, subSchema)) ->
                    match OrderedMap.tryFind prop o with
                    | Some v ->
                        { (validateSubSchema false $"{jsonPath}/{prop}" subSchema v) with
                            matchedPropertiesDeep = Set.singleton prop }
                    | None -> JsonSchemaResultData.empty)
                |> addMany
                |> fun result ->
                    { result with
                        matchedProperties = Set.ofSeq properties.Keys }
            | _ -> acc
        | PatternProperties patternProperties ->
            match json with
            | Object o ->
                patternProperties
                |> Seq.collect (fun (KeyValue(pattern, subSchema)) ->
                    o
                    |> Seq.filter (fun (KeyValue(prop, _)) ->
                        System.Text.RegularExpressions.Regex.IsMatch(prop, pattern))
                    |> Seq.map (fun (KeyValue(prop, v)) ->
                        { (validateSubSchema false $"{jsonPath}/{prop}" subSchema v) with
                            matchedPropertiesDeep = Set.singleton prop }))
                |> addMany
            | _ -> acc
        | AdditionalProperties subSchema ->
            match json with
            | Object o ->
                let propertiesSet = acc.matchedProperties

                o
                |> Seq.filter (fun (KeyValue(prop, _)) -> not (Set.contains prop propertiesSet))
                |> Seq.map (fun (KeyValue(prop, v)) ->
                    { (validateSubSchema false $"{jsonPath}/{prop}" subSchema v) with
                        matchedPropertiesDeep = Set.singleton prop })
                |> addMany
            | _ -> acc
        | DependentSchemas dependentSchemas ->
            match json with
            | Object o ->
                dependentSchemas
                |> Seq.map (fun (KeyValue(k, subSchema)) ->
                    match OrderedMap.tryFind k o with
                    | Some v ->
                        // Note: `k` itself doesn't seem to count as an 'evaluated' property..?
                        validateSubSchema isInsideRef jsonPath subSchema v
                    | None -> JsonSchemaResultData.empty)
                |> addMany
            | _ -> acc
        | UnevaluatedProperties subSchema ->
            match json with
            | Object o ->
                o
                |> Seq.filter (fun (KeyValue(prop, _)) -> not (Set.contains prop acc.matchedPropertiesDeep))
                |> Seq.map (fun (KeyValue(prop, v)) ->
                    { (validateSubSchema false $"{jsonPath}/{prop}" subSchema v) with
                        matchedPropertiesDeep = Set.empty })
                |> addMany
            | _ -> acc
        | Required required ->
            match json with
            | Object o ->
                validateSimple2 (fun () -> List.forall o.ContainsKey required) "Object is missing required properties"
                |> addOne
            | _ -> acc
        | PropertyNames subSchema ->
            match json with
            | Object o ->
                o
                |> Seq.map (fun (KeyValue(prop, _)) ->
                    { (validateSubSchema false $"{jsonPath}/{prop}" subSchema (String prop)) with
                        matchedPropertiesDeep = Set.empty })
                |> addMany
            | _ -> acc
        | MinProperties minProperties ->
            match json with
            | Object o ->
                validateSimple2 (fun () -> o.Count >= minProperties) "Object is too small"
                |> addOne
            | _ -> acc
        | MaxProperties maxProperties ->
            match json with
            | Object o ->
                validateSimple2 (fun () -> o.Count <= maxProperties) "Object is too large"
                |> addOne
            | _ -> acc
        | DependentRequired dependentRequired ->
            match json with
            | Object o ->
                dependentRequired
                |> Seq.collect (fun (KeyValue(k, required)) ->
                    match OrderedMap.tryFind k o with
                    | Some _ ->
                        required
                        |> Seq.filter (fun prop -> not (o.ContainsKey prop))
                        |> Seq.map (fun prop ->
                            JsonSchemaResultData.createFailedFromSingle
                                $"{schemaPath}/{k}"
                                $"{jsonPath}/{prop}"
                                (Error "Object is missing required properties"))
                    | None -> Seq.empty)
                |> addMany
            | _ -> acc
        | Invalid -> addOne (Error "This schema is always false")

    and validateSubSchema isInsideRef jsonPath (schema: JsonSubSchema) (json: JsonTree) : JsonSchemaResultData =
        schema
        |> List.fold (processSingle isInsideRef jsonPath json) JsonSchemaResultData.empty

    validateSubSchema false "" rootSchema.schema rootJson |> _.result
