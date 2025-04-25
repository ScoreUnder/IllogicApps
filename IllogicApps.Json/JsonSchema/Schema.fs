namespace IllogicApps.Json

open System.Collections.Immutable

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
