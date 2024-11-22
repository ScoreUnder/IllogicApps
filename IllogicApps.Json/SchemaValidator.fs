module IllogicApps.Json.SchemaValidator

type StringComparison = System.StringComparison

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
let rec jsonsEqual (a: JsonTree) (b: JsonTree) =
    match a, b with
    | JsonTree.Null, JsonTree.Null -> true
    | JsonTree.Boolean a, JsonTree.Boolean b -> a = b
    | JsonTree.String a, JsonTree.String b -> a.Equals(b, StringComparison.Ordinal)
    | NumbersAsInteger(a, b) -> a = b
    | NumbersAsFloat(a, b) -> a = b
    | NumbersAsDecimal(a, b) -> a = b
    | JsonTree.Array a, JsonTree.Array b -> a.Length = b.Length && Seq.forall2 jsonsEqual a b
    | JsonTree.Object a, JsonTree.Object b ->
        a.Count = b.Count
        && Seq.forall2
            (fun (k1: string) k2 -> k1.Equals(k2, StringComparison.Ordinal))
            (Seq.sort a.Keys)
            (Seq.sort b.Keys)
        && Seq.forall (fun k1 -> jsonsEqual a.[k1] b.[k1]) a.Keys
    | _ -> false

// 4.3.2 Boolean JSON Schemas
// We can abbreviate schema objects as booleans:
// true -> {}
// false -> {"not": {}}

// OK this is long i'm going to implement something minimal and then come back to this
type JsonSchema =
    { type_: SchemaType list option

      // Subschema combinators
      not: JsonSchema option
      allOf: JsonSchema list option
      anyOf: JsonSchema list option
      oneOf: JsonSchema list option

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
      additionalProperties: bool option
      unevaluatedProperties: bool option
      required: string list option
      propertyNames: JsonSchema option
      minProperties: int option
      maxProperties: int option }

let emptyJsonSchema =
    { type_ = None

      // Subschema combinators
      not = None
      allOf = None
      anyOf = None
      oneOf = None

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
      maxProperties = None }

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
          additionalProperties = JsonTree.tryGetKey "additionalProperties" json |> Option.map ensureBoolean
          unevaluatedProperties = JsonTree.tryGetKey "unevaluatedProperties" json |> Option.map ensureBoolean
          required = readOptionalStringList "required" json
          propertyNames = readOptionalSubSchema "propertyNames" json
          minProperties = readOptionalInt "minProperties" json
          maxProperties = readOptionalInt "maxProperties" json }
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

let private listOfDistinct eqf seq =
    // This is a naive/slow implementation, but probably not a problem for now
    Seq.fold (fun acc x -> if List.exists (eqf x) acc then acc else x :: acc) [] seq
    |> List.rev

let rec validateJsonSchema (schema: JsonSchema) (json: JsonTree) =
    Option.forall (List.exists (fun schemaType -> typesMatch schemaType json)) schema.type_
    && let validateWith subSchema = validateJsonSchema subSchema json in

       Option.forall (not << validateWith) schema.not
       && Option.forall (List.forall validateWith) schema.allOf
       && Option.forall (List.exists validateWith) schema.anyOf
       && Option.forall
           (fun subSchemas ->
               subSchemas
               |> List.sumBy (fun subSchema -> if validateWith subSchema then 1 else 0)
               |> (=) 1)
           schema.oneOf
       && match json with
          | String s ->
              Option.forall (fun minLength -> s.Length >= minLength) schema.minLength
              && Option.forall (fun maxLength -> s.Length <= maxLength) schema.maxLength
              && Option.forall (fun pattern -> System.Text.RegularExpressions.Regex.IsMatch(s, pattern)) schema.pattern
          | Integer _
          | Float _
          | Decimal _ ->
              let number = numberAsDecimal json in

              Option.forall (fun multipleOf -> number % multipleOf = 0m) schema.multipleOf
              && Option.forall (fun minimum -> number >= minimum) schema.minimum
              && Option.forall (fun exclusiveMinimum -> number > exclusiveMinimum) schema.exclusiveMinimum
              && Option.forall (fun maximum -> number <= maximum) schema.maximum
              && Option.forall (fun exclusiveMaximum -> number < exclusiveMaximum) schema.exclusiveMaximum
          | Array a ->
              Option.forall (fun subSchemas -> Seq.forall2 validateJsonSchema subSchemas a) schema.prefixItems
              && let numPrefixItems =
                  match schema.prefixItems with
                  | None -> 0
                  | Some prefixItems -> List.length prefixItems in

                 Option.forall
                     (fun subSchema -> a |> Seq.skip numPrefixItems |> Seq.forall (validateJsonSchema subSchema))
                     schema.items
                 && Option.forall
                     (fun subSchema ->
                         a
                         |> Seq.skip (* Todo: wrong number, needs to take subschemas into account *) numPrefixItems
                         |> Seq.forall (validateJsonSchema subSchema))
                     schema.unevaluatedItems
                 && let numContains =
                     Option.map (fun subSchema -> seqCount (validateJsonSchema subSchema) a) schema.contains in

                    numContains <> Some 0
                    && Option.forall
                        (fun minContains -> Option.forall (fun numContains -> numContains >= minContains) numContains)
                        schema.minContains
                    && Option.forall
                        (fun maxContains -> Option.forall (fun numContains -> numContains <= maxContains) numContains)
                        schema.maxContains
                    && Option.forall (fun minItems -> a.Length >= minItems) schema.minItems
                    && Option.forall (fun maxItems -> a.Length <= maxItems) schema.maxItems
                    && Option.forall
                        (fun uniqueItems ->
                            if uniqueItems then
                                a |> listOfDistinct jsonsEqual |> List.length = a.Length
                            else
                                true)
                        schema.uniqueItems
          | Object o ->
              Option.forall
                  (OrderedMap.forall (fun k subSchema ->
                      match OrderedMap.tryFind k o with
                      | Some v -> validateJsonSchema subSchema v
                      | None -> true))
                  schema.properties
              && Option.forall
                  (OrderedMap.forall (fun pattern subSchema ->
                      o
                      |> OrderedMap.toSeq
                      |> Seq.filter (fun (prop, _) -> System.Text.RegularExpressions.Regex.IsMatch(prop, pattern))
                      |> Seq.forall (fun (_, v) -> validateJsonSchema subSchema v)))
                  schema.patternProperties
              && if schema.additionalProperties = Some false then
                     true //TODO
                 // Checks for anything not in properties or patternProperties
                 else
                     true
              && if schema.unevaluatedProperties = Some false then
                     true //TODO
                 // Checks for anything not in properties or patternProperties,
                 // but this time it's recursive and will need a restructure of the
                 // function signature
                 else
                     true
              && Option.forall (List.forall o.ContainsKey) schema.required
              && Option.forall
                  (fun subSchema -> OrderedMap.forall (fun k _ -> validateJsonSchema subSchema (String k)) o)
                  schema.propertyNames
              && Option.forall (fun min -> o.Count >= min) schema.minProperties
              && Option.forall (fun max -> o.Count <= max) schema.maxProperties
          | _ -> true
