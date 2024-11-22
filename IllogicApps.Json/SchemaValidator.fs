module IllogicApps.Json.SchemaValidator

open System

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
      not: JsonSchema option
      allOf: JsonSchema list option
      anyOf: JsonSchema list option
      oneOf: JsonSchema list option
      items: JsonSchema option // for arrays
      properties: OrderedMap<string, JsonSchema> option // for objects
      required: string list option } // for objects

let emptyJsonSchema =
    { type_ = None

      // Subschema combinators
      not = None
      allOf = None
      anyOf = None
      oneOf = None

      // Array schemas
      items = None

      // Object schemas
      properties = None
      required = None }

let mapArrayOrSingle f =
    function
    | Array a -> a |> Seq.map f |> List.ofSeq
    | v -> [ f v ]


let rec jsonSchemaOfJson json =
    let readOptionalSubSchemas key json =
        JsonTree.tryGetKey key json
        |> Option.map (ensureArray >> Seq.map jsonSchemaOfJson >> List.ofSeq)

    match json with
    | Boolean true -> emptyJsonSchema
    | Boolean false ->
        { emptyJsonSchema with
            not = Some emptyJsonSchema }
    | Object _ ->
        { type_ =
            JsonTree.tryGetKey "type" json
            |> Option.map (mapArrayOrSingle (ensureString >> schemaTypeOfString))
          not = JsonTree.tryGetKey "not" json |> Option.map jsonSchemaOfJson
          allOf = readOptionalSubSchemas "allOf" json
          anyOf = readOptionalSubSchemas "anyOf" json
          oneOf = readOptionalSubSchemas "oneOf" json
          items = JsonTree.tryGetKey "items" json |> Option.map jsonSchemaOfJson
          properties =
            JsonTree.tryGetKey "properties" json
            |> Option.map (ensureObject >> OrderedMap.mapValuesOnly jsonSchemaOfJson)
          required =
            JsonTree.tryGetKey "required" json
            |> Option.map (ensureArray >> Seq.map ensureString >> List.ofSeq) }
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
    | SchemaType.Integer, Float f when Double.IsInteger(f) -> true
    | SchemaType.Integer, Decimal d when Decimal.Truncate(d) = d -> true
    | SchemaType.String, String _ -> true
    | _ -> false

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
          | Array a -> Option.forall (fun subSchema -> a |> Seq.forall (validateJsonSchema subSchema)) schema.items
          | Object o ->
              Option.forall
                  (OrderedMap.forall (fun k subSchema ->
                      match OrderedMap.tryFind k o with
                      | Some v -> validateJsonSchema subSchema v
                      | None -> true))
                  schema.properties
              && Option.forall (List.forall o.ContainsKey) schema.required
          | _ -> true
