module IllogicApps.Compat.LogicAppUnit.NewtonsoftJsonConversions

open System
open IllogicApps.Json
open Newtonsoft.Json.Linq

let rec illogicJsonOfNewtonsoftJson (json: JToken) : JsonTree =
    match json.Type with
    | JTokenType.Object ->
        json.Children<JProperty>()
        |> Seq.map (fun p -> p.Name, illogicJsonOfNewtonsoftJson p.Value)
        |> Conversions.createObject
    | JTokenType.Array ->
        json.Children()
        |> Seq.map illogicJsonOfNewtonsoftJson
        |> Conversions.createArray
    | JTokenType.Integer -> Integer(json.Value<int64>())
    | JTokenType.Float ->
        match json with
        | :? JValue as jValue ->
            match jValue.Value with
            | :? Half as h -> Float(float h)
            | :? single as s -> Float(float s)
            | :? float as f -> Float(f)
            | :? decimal as d -> Decimal(d)
            | _ -> Float(json.Value<float>())
        | _ -> Float(json.Value<float>())
    | JTokenType.String -> String(json.Value<string>())
    | JTokenType.Boolean -> Boolean(json.Value<bool>())
    | JTokenType.Null -> Null
    | JTokenType.Date -> String(json.Value<DateTime>().ToString("o"))
    | JTokenType.Guid -> String(json.Value<Guid>().ToString())
    | JTokenType.Uri -> String(json.Value<Uri>().ToString())
    | JTokenType.TimeSpan -> String(json.Value<TimeSpan>().ToString())
    | typ -> failwithf "Cannot convert Newtonsoft.Json token of type %O" typ

let rec newtonsoftJsonOfIllogicJson (json: JsonTree) : JToken =
    match json with
    | Object props ->
        props
        |> OrderedMap.toSeq
        |> Seq.map (fun (k, v) -> JProperty(k, newtonsoftJsonOfIllogicJson v))
        |> JObject
        :> JToken
    | Array items -> items |> Seq.map newtonsoftJsonOfIllogicJson |> JArray :> JToken
    | Integer i -> JValue(i)
    | Float f -> JValue(f)
    | Decimal d -> JValue(d)
    | String s -> JValue(s)
    | Boolean b -> JValue(b)
    | Null -> JValue.CreateNull()
