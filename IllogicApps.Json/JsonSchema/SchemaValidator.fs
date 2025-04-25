module IllogicApps.Json.SchemaValidator

type StringComparison = System.StringComparison
type SortedSet<'T> = System.Collections.Generic.SortedSet<'T>
type IComparer<'T> = System.Collections.Generic.IComparer<'T>

open IllogicApps.Json.Conversions

let private countDistinct (cmpf: 'a -> 'a -> int) (seq: 'a seq) =
    let comparer =
        { new IComparer<'a> with
            member this.Compare(x, y) = cmpf x y }

    let seenSet = SortedSet<'a>(comparer)
    seenSet.UnionWith seq
    seenSet.Count

type JsonSchemaSingleResult =
    | Ok
    | Error of string
    | Warning of string

type JsonSchemaResultMessage =
    { [<CompiledName("SchemaPath")>]
      schemaPath: string
      [<CompiledName("JsonPath")>]
      jsonPath: string
      [<CompiledName("Result")>]
      result: JsonSchemaSingleResult }

type JsonSchemaResult =
    { [<CompiledName("Messages")>]
      messages: JsonSchemaResultMessage list
      [<CompiledName("IsMatch")>]
      isMatch: bool }

module JsonSchemaResult =
    let empty = { messages = []; isMatch = true }

    let merge a b =
        if LanguagePrimitives.PhysicalEquality a empty then
            b
        else
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
        | Warning v ->
            { messages =
                { schemaPath = schemaPath
                  jsonPath = jsonPath
                  result = Warning v }
                :: result.messages
              isMatch = result.isMatch }
        | Ok -> result

    let mergeMany results result = List.fold merge result results

    let formatMessages messages =
        messages
        |> List.map (fun m ->
            match m.result with
            | Ok -> ""
            | Warning v -> $"Warning: At schema#{m.schemaPath}, json#{m.jsonPath}: {v}"
            | Error v -> $"Error: At schema#{m.schemaPath} json#{m.jsonPath}: {v}")
        |> String.concat "\n"

type private JsonSchemaResultData =
    { result: JsonSchemaResult
      matchedItemsDeep: int Set
      matchedPropertiesDeep: string Set }

module private JsonSchemaResultData =
    let empty =
        { result = JsonSchemaResult.empty
          matchedItemsDeep = Set.empty
          matchedPropertiesDeep = Set.empty }

    let merge newResult origResult =
        if LanguagePrimitives.PhysicalEquality newResult empty then
            origResult
        else if newResult.result.isMatch then
            { origResult with
                result = JsonSchemaResult.merge newResult.result origResult.result
                matchedItemsDeep = max newResult.matchedItemsDeep origResult.matchedItemsDeep
                matchedPropertiesDeep = Set.union newResult.matchedPropertiesDeep origResult.matchedPropertiesDeep }
        else
            { origResult with
                result = JsonSchemaResult.merge newResult.result origResult.result }

    let add schemaPath jsonPath (single: JsonSchemaSingleResult) result =
        match single with
        | Ok -> result
        | _ ->
            { result with
                JsonSchemaResultData.result = JsonSchemaResult.add schemaPath jsonPath single result.result }

    let mergeMany results origResult =
        PerfSeq.fold (fun acc el -> merge el acc) origResult results

    let inline mergeManyMapi
        ([<InlineIfLambda>] f: int -> 'a -> JsonSchemaResultData)
        (results: 'a seq)
        (origResult: JsonSchemaResultData)
        =
        PerfSeq.foldi (fun i acc el -> merge (f i el) acc) origResult results

    let createFailedFromSingle schemaPath jsonPath value =
        { empty with
            result =
                { messages =
                    [ { schemaPath = schemaPath
                        jsonPath = jsonPath
                        result = value } ]
                  isMatch = false } }

type private JsonSchemaResultState =
    { current: JsonSchemaResultData
      prefixLength: int
      minContains: int
      maxContains: int option
      matchedProperties: string Set }

module private JsonSchemaResultState =
    let empty =
        { current = JsonSchemaResultData.empty
          prefixLength = 0
          minContains = 1
          maxContains = None
          matchedProperties = Set.empty }

    let add schemaPath jsonPath result state =
        let added = JsonSchemaResultData.add schemaPath jsonPath result state.current

        if LanguagePrimitives.PhysicalEquality state.current added then
            state
        else
            { state with current = added }

    let merge result state =
        let merged = JsonSchemaResultData.merge result state.current

        if LanguagePrimitives.PhysicalEquality state.current merged then
            state
        else
            { state with current = merged }

    let mergeMany results state =
        let merged = JsonSchemaResultData.mergeMany results state.current

        if LanguagePrimitives.PhysicalEquality state.current merged then
            state
        else
            { state with current = merged }

    let inline mergeManyMapi
        ([<InlineIfLambda>] f: int -> 'a -> JsonSchemaResultData)
        (results: 'a seq)
        (state: JsonSchemaResultState)
        =
        let merged = JsonSchemaResultData.mergeManyMapi f results state.current

        if LanguagePrimitives.PhysicalEquality state.current merged then
            state
        else
            { state with current = merged }

let private resolveRef (schema: JsonSchema) (refName: string) =
    if refName = "#" then
        Result.Ok schema.schema
    else
        match Map.tryFind refName schema.subSchemas with
        | Some subSchema -> Result.Ok subSchema
        | None -> Result.Error $"Reference {refName} not found in schema"

let inline private validateSimple2 ([<InlineIfLambda>] f: unit -> bool) ([<InlineIfLambda>] message: unit -> string) =
    if f () then Ok else Error(message ())

let private validateSimple f message =
    function
    | None -> Ok
    | Some v when f v -> Ok
    | _ -> Error message

[<CompiledName("Validate")>]
let validate (rootSchema: JsonSchema) (rootJson: JsonTree) : JsonSchemaResult =
    let rec processSingle
        isInsideRef
        jsonPath
        json
        (acc: JsonSchemaResultState)
        (schemaPath, schemaExec)
        : JsonSchemaResultState =
        let inline addOne result =
            JsonSchemaResultState.add schemaPath jsonPath result acc

        let inline addFull result = JsonSchemaResultState.merge result acc

        let inline addMany results =
            JsonSchemaResultState.mergeMany results acc

        let inline addManyMapi f results =
            JsonSchemaResultState.mergeManyMapi f results acc

        match schemaExec with
        | Not subSchema ->
            validateSimple2
                (fun () -> not (validateSubSchema isInsideRef jsonPath subSchema json).result.isMatch)
                (fun () -> "should not have validated, but did")
            |> addOne
        | AllOf subSchemas ->
            subSchemas
            |> addManyMapi (fun _ subSchema -> validateSubSchema isInsideRef jsonPath subSchema json)
        | AnyOf subSchemas ->
            let rec validateAnyOf' acc ind =
                if ind = subSchemas.Length then
                    JsonSchemaResultData.mergeMany
                        acc
                        (JsonSchemaResultData.createFailedFromSingle schemaPath jsonPath (Error "No match"))
                else
                    let subSchema = subSchemas.[ind]
                    let result = validateSubSchema isInsideRef jsonPath subSchema json

                    if result.result.isMatch then
                        result
                    else
                        validateAnyOf' (result :: acc) (ind + 1)

            validateAnyOf' [] 0 |> addFull
        | OneOf subSchemas ->
            let rec validateOneOf' (acc: Result<JsonSchemaResultData, JsonSchemaResultData>) ind =
                if ind = subSchemas.Length then
                    match acc with
                    | Result.Ok result -> result
                    | Result.Error fails ->
                        JsonSchemaResultData.merge
                            fails
                            (JsonSchemaResultData.createFailedFromSingle schemaPath jsonPath (Error "No match"))
                else
                    let subSchema = subSchemas.[ind]
                    let result = validateSubSchema isInsideRef jsonPath subSchema json
                    let isMatch = result.result.isMatch

                    match acc with
                    | Result.Error _ when isMatch -> validateOneOf' (Result.Ok result) (ind + 1)
                    | Result.Error fails ->
                        validateOneOf' (Result.Error(JsonSchemaResultData.merge result fails)) (ind + 1)
                    | Result.Ok _ when isMatch ->
                        JsonSchemaResultData.createFailedFromSingle schemaPath jsonPath (Error "More than one match")
                    | Result.Ok _ -> validateOneOf' acc (ind + 1)

            validateOneOf' (Result.Error JsonSchemaResultData.empty) 0 |> addFull
        | Enum values ->
            validateSimple2 (fun () -> PerfSeq.exists (JsonCompare.jsonsEqual json) values) (fun () ->
                "Enum value not correct")
            |> addOne
        | Const value ->
            validateSimple2 (fun () -> JsonCompare.jsonsEqual json value) (fun () -> "Const value not correct")
            |> addOne
        | TypeTest types ->
            validateSimple2 (fun () -> PerfSeq.exists (fun t -> JsonCompare.typesMatch t json) types) (fun () ->
                "Type not correct")
            |> addOne
        | IfThenElse(cond, thenBlock, elseBlock) ->
            if (validateSubSchema isInsideRef jsonPath cond json).result.isMatch then
                validateSubSchema isInsideRef jsonPath thenBlock json
            else
                validateSubSchema isInsideRef jsonPath elseBlock json
            |> addFull
        | Ref refName ->
            if isInsideRef then
                Result.Error "Not allowed to nest refs (infinite recursion possible)"
            else
                resolveRef rootSchema refName
                |> Result.map (fun schema -> validateSubSchema true jsonPath schema json)
            |> Result.defaultWith (fun err ->
                JsonSchemaResultData.createFailedFromSingle schemaPath jsonPath (Warning err))
            |> addFull
        | MinLength minLength ->
            match json with
            | String s ->
                validateSimple2 (fun () -> s.Length >= minLength) (fun () -> "String is too short")
                |> addOne
            | _ -> acc
        | MaxLength maxLength ->
            match json with
            | String s ->
                validateSimple2 (fun () -> s.Length <= maxLength) (fun () -> "String is too long")
                |> addOne
            | _ -> acc
        | Pattern pattern ->
            match json with
            | String s ->
                validateSimple2 (fun () -> System.Text.RegularExpressions.Regex.IsMatch(s, pattern)) (fun () ->
                    "String pattern does not match")
                |> addOne
            | _ -> acc
        | MultipleOf multipleOf ->
            match json with
            | Integer _
            | Float _
            | Decimal _ ->
                let number = numberAsDecimal json in

                validateSimple2 (fun () -> number % multipleOf = 0m) (fun () ->
                    "Number is not a multiple of multipleOf")
                |> addOne
            | _ -> acc
        | Minimum minimum ->
            match json with
            | Integer _
            | Float _
            | Decimal _ ->
                let number = numberAsDecimal json in

                validateSimple2 (fun () -> number >= minimum) (fun () -> "Number is less than minimum")
                |> addOne
            | _ -> acc
        | ExclusiveMinimum exclusiveMinimum ->
            match json with
            | Integer _
            | Float _
            | Decimal _ ->
                let number = numberAsDecimal json in

                validateSimple2 (fun () -> number > exclusiveMinimum) (fun () ->
                    "Number is not greater than exclusiveMinimum")
                |> addOne
            | _ -> acc
        | Maximum maximum ->
            match json with
            | Integer _
            | Float _
            | Decimal _ ->
                let number = numberAsDecimal json in

                validateSimple2 (fun () -> number <= maximum) (fun () -> "Number is greater than maximum")
                |> addOne
            | _ -> acc
        | ExclusiveMaximum exclusiveMaximum ->
            match json with
            | Integer _
            | Float _
            | Decimal _ ->
                let number = numberAsDecimal json in

                validateSimple2 (fun () -> number < exclusiveMaximum) (fun () ->
                    "Number is not less than exclusiveMaximum")
                |> addOne
            | _ -> acc
        | PrefixItems subSchemas ->
            match json with
            | Array a ->
                subSchemas
                |> addManyMapi (fun i subSchema ->
                    let json = a.[i]

                    { (validateSubSchema false $"{jsonPath}/{i}" subSchema json) with
                        matchedItemsDeep = Set.singleton i })
                |> fun result ->
                    { result with
                        prefixLength = subSchemas.Length }
            | _ -> acc
        | PrefixItemsAll subSchema ->
            match json with
            | Array a ->
                a
                |> addManyMapi (fun i json ->
                    { (validateSubSchema false $"{jsonPath}/{i}" subSchema json) with
                        matchedItemsDeep = Set.singleton i })
            | _ -> acc
        | Items subSchema ->
            let numPrefixItems = acc.prefixLength

            match json with
            | Array a when a.Length > numPrefixItems ->
                a
                |> Seq.skip acc.prefixLength
                |> addManyMapi (fun i json ->
                    { (validateSubSchema false $"{jsonPath}/{i + numPrefixItems}" subSchema json) with
                        matchedItemsDeep = Set.singleton (i + numPrefixItems) })
            | _ -> acc
        | Contains subSchema ->
            match json with
            | Array a ->
                let containsCount, containsResult =
                    a
                    |> Seq.mapi (fun i json ->
                        { (validateSubSchema false $"{jsonPath}/{i}" subSchema json) with
                            matchedItemsDeep = Set.singleton i })
                    |> PerfSeq.fold
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
                let matchedItemsDeep = acc.current.matchedItemsDeep

                a
                |> addManyMapi (fun i json ->
                    if Set.contains i matchedItemsDeep then
                        JsonSchemaResultData.empty
                    else
                        { (validateSubSchema false $"{jsonPath}/{i}" subSchema json) with
                            matchedItemsDeep = Set.empty })
            | _ -> acc
        | MinItems minItems ->
            match json with
            | Array a ->
                validateSimple2 (fun () -> a.Length >= minItems) (fun () -> "Array is too short")
                |> addOne
            | _ -> acc
        | MaxItems maxItems ->
            match json with
            | Array a ->
                validateSimple2 (fun () -> a.Length <= maxItems) (fun () -> "Array is too long")
                |> addOne
            | _ -> acc
        | UniqueItems ->
            match json with
            | Array a ->
                validateSimple2 (fun () -> countDistinct JsonCompare.compareJsons a = a.Length) (fun () ->
                    "Array contains duplicate items")
                |> addOne
            | _ -> acc
        | Properties properties ->
            match json with
            | Object o ->
                properties
                |> addManyMapi (fun _ (KeyValue(prop, subSchema)) ->
                    match OrderedMap.tryFind prop o with
                    | Some v ->
                        { (validateSubSchema false $"{jsonPath}/{prop}" subSchema v) with
                            matchedPropertiesDeep = Set.singleton prop }
                    | None -> JsonSchemaResultData.empty)
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
                |> addManyMapi (fun _ (KeyValue(prop, v)) ->
                    { (validateSubSchema false $"{jsonPath}/{prop}" subSchema v) with
                        matchedPropertiesDeep = Set.singleton prop })
            | _ -> acc
        | DependentSchemas dependentSchemas ->
            match json with
            | Object o ->
                dependentSchemas
                |> addManyMapi (fun _ (KeyValue(k, subSchema)) ->
                    match OrderedMap.tryFind k o with
                    | Some v ->
                        // Note: `k` itself doesn't seem to count as an 'evaluated' property..?
                        validateSubSchema isInsideRef jsonPath subSchema v
                    | None -> JsonSchemaResultData.empty)
            | _ -> acc
        | UnevaluatedProperties subSchema ->
            match json with
            | Object o ->
                let matchedPropertiesDeep = acc.current.matchedPropertiesDeep

                o
                |> Seq.filter (fun (KeyValue(prop, _)) -> not (Set.contains prop matchedPropertiesDeep))
                |> addManyMapi (fun _ (KeyValue(prop, v)) ->
                    { (validateSubSchema false $"{jsonPath}/{prop}" subSchema v) with
                        matchedPropertiesDeep = Set.empty })
            | _ -> acc
        | Required required ->
            match json with
            | Object o ->
                validateSimple2 (fun () -> PerfSeq.forall o.ContainsKey required) (fun () ->
                    "Object is missing required properties")
                |> addOne
            | _ -> acc
        | PropertyNames subSchema ->
            match json with
            | Object o ->
                o
                |> addManyMapi (fun _ (KeyValue(prop, _)) ->
                    { (validateSubSchema false $"{jsonPath}/{prop}" subSchema (String prop)) with
                        matchedPropertiesDeep = Set.empty })
            | _ -> acc
        | MinProperties minProperties ->
            match json with
            | Object o ->
                validateSimple2 (fun () -> o.Count >= minProperties) (fun () -> "Object is too small")
                |> addOne
            | _ -> acc
        | MaxProperties maxProperties ->
            match json with
            | Object o ->
                validateSimple2 (fun () -> o.Count <= maxProperties) (fun () -> "Object is too large")
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
        |> PerfSeq.fold (processSingle isInsideRef jsonPath json) JsonSchemaResultState.empty
        |> _.current

    validateSubSchema false "" rootSchema.schema rootJson |> _.result
