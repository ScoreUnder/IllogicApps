namespace IllogicApps.Json

open System.Collections.Generic
open System.Collections.Immutable
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

    [<CompiledName("OfString")>]
    static member ofString =
        function
        | "null" -> SchemaType.Null
        | "boolean" -> SchemaType.Boolean
        | "object" -> SchemaType.Object
        | "array" -> SchemaType.Array
        | "number" -> SchemaType.Number
        | "integer" -> SchemaType.Integer
        | "string" -> SchemaType.String
        | v -> failwithf "Unknown schema type: %s" v

type JsonSchemaExec =
    // Subschema combinators
    | Not of JsonSubSchema
    | AllOf of JsonSubSchema ImmutableArray
    | AnyOf of JsonSubSchema ImmutableArray
    | OneOf of JsonSubSchema ImmutableArray
    | IfThenElse of JsonSubSchema * JsonSubSchema * JsonSubSchema

    // Generic schemas
    | Invalid
    | TypeTest of SchemaType ImmutableArray
    | Enum of JsonTree ImmutableArray
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
    | PrefixItems of JsonSubSchema ImmutableArray
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
    | Required of string ImmutableArray
    | PropertyNames of JsonSubSchema
    | MinProperties of int
    | MaxProperties of int
    | DependentRequired of OrderedMap<string, string ImmutableArray>
    | DependentSchemas of OrderedMap<string, JsonSubSchema>

    // Defs and refs
    | Ref of string

and JsonSubSchema = (string * JsonSchemaExec) ImmutableArray

and JsonSchema =
    { schema: JsonSubSchema
      subSchemas: Map<string, JsonSubSchema> }

    [<CompiledName("Empty")>]
    static member empty =
        { schema = ImmutableArray.Empty
          subSchemas = Map.empty }

module JsonSubSchema =
    [<CompiledName("AlwaysTrue")>]
    let alwaysTrue: JsonSubSchema = ImmutableArray.Empty

    [<CompiledName("AlwaysFalse")>]
    let alwaysFalse schemaPath : JsonSubSchema =
        ImmutableArray.Create((schemaPath, Invalid))

type private BuildJsonSubSchema = (string * JsonSchemaExec) list

module JsonSchema =
    let private mapArrayOrSingle (f: JsonTree -> 'a) =
        function
        | Array a -> a |> Seq.map f |> ImmutableArray.CreateRange
        | v -> ImmutableArray.Create(f v)

    type private SubSchemaParseState =
        { ifCond: JsonSubSchema option
          thenBlock: JsonSubSchema option
          elseBlock: JsonSubSchema option }

    [<RequireQualifiedAccess>]
    type private SubSchemaKeyType =
        | EarlySubSchema
        | MidSubSchema
        | LateSubSchema
        | EarlyMultiSubSchema
        | PropertySchema
        | IntNumeric
        | FloatNumeric
        | Type
        | PrefixItems
        | If
        | Then
        | Else
        | Enum
        | Const
        | Pattern
        | UniqueItems
        | Required
        | DependentRequired
        | Ref
        | Unknown

    let private subSchemaKeyMap =
        Dictionary<string, SubSchemaKeyType>(
            [| "type", SubSchemaKeyType.Type
               "not", SubSchemaKeyType.EarlySubSchema
               "propertyNames", SubSchemaKeyType.EarlySubSchema
               "unevaluatedItems", SubSchemaKeyType.LateSubSchema
               "unevaluatedProperties", SubSchemaKeyType.LateSubSchema
               "items", SubSchemaKeyType.MidSubSchema
               "contains", SubSchemaKeyType.MidSubSchema
               "additionalProperties", SubSchemaKeyType.MidSubSchema
               "anyOf", SubSchemaKeyType.EarlyMultiSubSchema
               "allOf", SubSchemaKeyType.EarlyMultiSubSchema
               "oneOf", SubSchemaKeyType.EarlyMultiSubSchema
               "prefixItems", SubSchemaKeyType.PrefixItems
               "if", SubSchemaKeyType.If
               "then", SubSchemaKeyType.Then
               "else", SubSchemaKeyType.Else
               "properties", SubSchemaKeyType.PropertySchema
               "patternProperties", SubSchemaKeyType.PropertySchema
               "dependentSchemas", SubSchemaKeyType.PropertySchema
               "enum", SubSchemaKeyType.Enum
               "const", SubSchemaKeyType.Const
               "minLength", SubSchemaKeyType.IntNumeric
               "maxLength", SubSchemaKeyType.IntNumeric
               "minContains", SubSchemaKeyType.IntNumeric
               "maxContains", SubSchemaKeyType.IntNumeric
               "minItems", SubSchemaKeyType.IntNumeric
               "maxItems", SubSchemaKeyType.IntNumeric
               "minProperties", SubSchemaKeyType.IntNumeric
               "maxProperties", SubSchemaKeyType.IntNumeric
               "pattern", SubSchemaKeyType.Pattern
               "multipleOf", SubSchemaKeyType.FloatNumeric
               "minimum", SubSchemaKeyType.FloatNumeric
               "exclusiveMinimum", SubSchemaKeyType.FloatNumeric
               "maximum", SubSchemaKeyType.FloatNumeric
               "exclusiveMaximum", SubSchemaKeyType.FloatNumeric
               "uniqueItems", SubSchemaKeyType.UniqueItems
               "required", SubSchemaKeyType.Required
               "dependentRequired", SubSchemaKeyType.DependentRequired
               "$ref", SubSchemaKeyType.Ref |]
            |> Seq.map KeyValuePair<string, SubSchemaKeyType>
        )

    /// <summary>Parse a <see cref="JsonSubSchema"/> from raw JSON.</summary>
    /// <returns>the parsed sub-schema and a set of all referenced sub-schemas</returns>
    [<CompiledName("ParseSubSchema")>]
    let rec subSchemaOfJson (schemaPath: string) (json: JsonTree) : JsonSubSchema * string Set =
        match json with
        | Boolean true -> JsonSubSchema.alwaysTrue, Set.empty
        | Boolean false -> JsonSubSchema.alwaysFalse schemaPath, Set.empty
        | Object o ->
            o
            |> OrderedMap.unorderedFold
                (fun
                    ((execFirst: BuildJsonSubSchema,
                      execMid: BuildJsonSubSchema,
                      execLast: BuildJsonSubSchema,
                      refs: string Set,
                      state: SubSchemaParseState) as acc)
                    k
                    v ->
                    match subSchemaKeyMap.GetValueOrDefault(k, SubSchemaKeyType.Unknown) with
                    | SubSchemaKeyType.Type ->
                        let types = v |> mapArrayOrSingle (ensureString >> SchemaType.ofString)
                        ($"{schemaPath}/{k}", TypeTest types) :: execFirst, execMid, execLast, refs, state
                    | SubSchemaKeyType.EarlySubSchema ->
                        let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}" v

                        let exec =
                            match k with
                            | "not" -> Not subSchema
                            | "propertyNames" -> PropertyNames subSchema
                            | _ -> failwith "Internal error: missing case"

                        ($"{schemaPath}/{k}", exec) :: execFirst, execMid, execLast, Set.union refs2 refs, state
                    | SubSchemaKeyType.LateSubSchema ->
                        let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}" v

                        let exec =
                            match k with
                            | "unevaluatedItems" -> UnevaluatedItems subSchema
                            | "unevaluatedProperties" -> UnevaluatedProperties subSchema
                            | _ -> failwith "Internal error: missing case"

                        execFirst, execMid, ($"{schemaPath}/{k}", exec) :: execLast, Set.union refs2 refs, state
                    | SubSchemaKeyType.MidSubSchema ->
                        let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}" v

                        let exec =
                            match k with
                            | "items" -> Items subSchema
                            | "contains" -> Contains subSchema
                            | "additionalProperties" -> AdditionalProperties subSchema
                            | _ -> failwith "Internal error: missing case"

                        execFirst, ($"{schemaPath}/{k}", exec) :: execMid, execLast, Set.union refs2 refs, state
                    | SubSchemaKeyType.EarlyMultiSubSchema ->
                        let _, subSchemas, refs2 =
                            v
                            |> ensureArray
                            |> PerfSeq.fold
                                (fun (i, subSchemas: ImmutableArray<JsonSubSchema>.Builder, refs) x ->
                                    let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}/{i}" x
                                    subSchemas.Add(subSchema) // NOTE: Mutation in fold
                                    i + 1, subSchemas, Set.union refs2 refs)
                                (0, ImmutableArray.CreateBuilder(), Set.empty)

                        let subSchemas = subSchemas.DrainToImmutable()

                        let exec =
                            match k with
                            | "anyOf" -> AnyOf subSchemas
                            | "allOf" -> AllOf subSchemas
                            | "oneOf" -> OneOf subSchemas
                            | _ -> failwith "Internal error: missing case"

                        ($"{schemaPath}/{k}", exec) :: execFirst, execMid, execLast, Set.union refs2 refs, state
                    | SubSchemaKeyType.PrefixItems ->
                        // Differs slightly from the above case in that we can have a single
                        // schema instead of an array, in which case it is applied to all
                        // items in the array.
                        match v with
                        | Array a ->
                            let _, subSchemas, refs2 =
                                a
                                |> PerfSeq.fold
                                    (fun (i, subSchemas: ImmutableArray<JsonSubSchema>.Builder, refs) x ->
                                        let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}/{i}" x
                                        subSchemas.Add(subSchema) // NOTE: Mutation in fold
                                        i + 1, subSchemas, Set.union refs2 refs)
                                    (0, ImmutableArray.CreateBuilder(), Set.empty)

                            let subSchemas = subSchemas.DrainToImmutable()

                            let execFirst = ($"{schemaPath}/{k}", PrefixItems subSchemas) :: execFirst
                            execFirst, execMid, execLast, Set.union refs2 refs, state
                        | _ ->
                            let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}" v

                            let execFirst = ($"{schemaPath}/{k}", PrefixItemsAll subSchema) :: execFirst
                            execFirst, execMid, execLast, Set.union refs2 refs, state
                    | SubSchemaKeyType.If ->
                        let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}" v
                        let state = { state with ifCond = Some subSchema }
                        execFirst, execMid, execLast, Set.union refs2 refs, state
                    | SubSchemaKeyType.Then ->
                        let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}" v

                        let state =
                            { state with
                                thenBlock = Some subSchema }

                        execFirst, execMid, execLast, Set.union refs2 refs, state
                    | SubSchemaKeyType.Else ->
                        let subSchema, refs2 = subSchemaOfJson $"{schemaPath}/{k}" v

                        let state =
                            { state with
                                elseBlock = Some subSchema }

                        execFirst, execMid, execLast, Set.union refs2 refs, state
                    | SubSchemaKeyType.PropertySchema ->
                        let subSchemaMap, refs2 =
                            v
                            |> ensureObject
                            |> OrderedMap.unorderedFold
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
                    | SubSchemaKeyType.Enum ->
                        let enumValues = v |> ensureArray
                        ($"{schemaPath}/{k}", Enum enumValues) :: execFirst, execMid, execLast, refs, state
                    | SubSchemaKeyType.Const -> ($"{schemaPath}/{k}", Const v) :: execFirst, execMid, execLast, refs, state
                    | SubSchemaKeyType.IntNumeric ->
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
                    | SubSchemaKeyType.Pattern ->
                        let value = v |> ensureString
                        execFirst, ($"{schemaPath}/{k}", Pattern value) :: execMid, execLast, refs, state
                    | SubSchemaKeyType.FloatNumeric ->
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
                    | SubSchemaKeyType.UniqueItems ->
                        let value = v |> ensureBoolean

                        if value then
                            execFirst, ($"{schemaPath}/{k}", UniqueItems) :: execMid, execLast, refs, state
                        else
                            acc
                    | SubSchemaKeyType.Required ->
                        let requiredProperties =
                            v |> ensureArray |> Seq.map ensureString |> ImmutableArray.CreateRange

                        execFirst, ($"{schemaPath}/{k}", Required requiredProperties) :: execMid, execLast, refs, state
                    | SubSchemaKeyType.DependentRequired ->
                        let dependentRequired =
                            v
                            |> ensureObject
                            |> OrderedMap.mapValuesOnly (fun v ->
                                v |> ensureArray |> Seq.map ensureString |> ImmutableArray.CreateRange)

                        let execMid = ($"{schemaPath}/{k}", DependentRequired dependentRequired) :: execMid
                        execFirst, execMid, execLast, refs, state
                    | SubSchemaKeyType.Ref ->
                        let ref = v |> ensureString
                        ($"{schemaPath}/{k}", Ref ref) :: execFirst, execMid, execLast, Set.add ref refs, state
                    | SubSchemaKeyType.Unknown ->
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
                              state.thenBlock |> Option.defaultValue JsonSubSchema.alwaysTrue,
                              state.elseBlock |> Option.defaultValue JsonSubSchema.alwaysTrue
                          ) ]

                let builder = ImmutableArray.CreateBuilder<string * JsonSchemaExec>()
                builder.AddRange ifBlock
                builder.AddRange execFirst
                builder.AddRange execMid
                builder.AddRange execLast
                builder.DrainToImmutable(), refs

        | _ ->
            failwith
                $"Invalid JSON schema: Expected an object or boolean at '{schemaPath}', but got {JsonTree.getType json} instead."

    /// <summary>Parse a <see cref="JsonSchema"/> from raw JSON.</summary>
    [<CompiledName("Parse")>]
    let ofJson json =
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
                |> PerfSeq.fold
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
                    |> PerfSeq.fold (fun acc (name, (schema, _)) -> Map.add name schema acc) known

                resolveRefs nextRefs nextKnown

        let initialSubSchema, initialRefs = subSchemaOfJson "" json

        { schema = initialSubSchema
          subSchemas = resolveRefs initialRefs Map.empty }
