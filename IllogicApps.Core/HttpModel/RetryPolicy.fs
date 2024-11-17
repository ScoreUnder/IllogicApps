module IllogicApps.Core.HttpModel.RetryPolicy

open System
open System.Xml
open IllogicApps.Json

let private timeSpanOfJson json =
    Conversions.ensureString json |> XmlConvert.ToTimeSpan

let private jsonOfTimeSpan (timeSpan: TimeSpan) = XmlConvert.ToString timeSpan |> String

type BackoffPolicy = { count: int64; interval: TimeSpan }

let addBackoffPolicyToJsonBuilder backoff (builder: OrderedMap.Builder<string, JsonTree>) =
    builder
        .Add("count", Integer backoff.count)
        .Add("interval", String(XmlConvert.ToString backoff.interval))

let backoffPolicyOfJson json =
    { count = JsonTree.getKey "count" json |> Conversions.ensureInteger
      interval = JsonTree.getKey "interval" json |> timeSpanOfJson }

type BackoffBoundsPolicy =
    { minimumInterval: TimeSpan option
      maximumInterval: TimeSpan option }

let addBackoffBoundsPolicyToJsonBuilder bounds (builder: OrderedMap.Builder<string, JsonTree>) =
    builder
        .MaybeAdd("minimumInterval", bounds.minimumInterval |> Option.map jsonOfTimeSpan)
        .MaybeAdd("maximumInterval", bounds.maximumInterval |> Option.map jsonOfTimeSpan)

let backoffBoundsPolicyOfJson json =
    { minimumInterval = JsonTree.tryGetKey "minimumInterval" json |> Option.map timeSpanOfJson
      maximumInterval = JsonTree.tryGetKey "maximumInterval" json |> Option.map timeSpanOfJson }

type RetryPolicy =
    | NoRetry
    | ExponentialBackoff of BackoffPolicy * BackoffBoundsPolicy
    | FixedBackoff of BackoffPolicy

let stringOfRetryPolicyType =
    function
    | NoRetry -> "none"
    | ExponentialBackoff _ -> "exponential"
    | FixedBackoff _ -> "fixed"

let jsonOfRetryPolicy retryPolicy =
    let builder =
        OrderedMap.Builder().Add("type", String(stringOfRetryPolicyType retryPolicy))

    match retryPolicy with
    | NoRetry -> builder
    | ExponentialBackoff(backoff, bounds) ->
        builder
        |> addBackoffPolicyToJsonBuilder backoff
        |> addBackoffBoundsPolicyToJsonBuilder bounds
    | FixedBackoff backoff -> builder |> addBackoffPolicyToJsonBuilder backoff
    |> _.Build()
    |> Object

let retryPolicyOfJson json =
    let type_ = JsonTree.getKey "type" json |> Conversions.ensureString

    match type_ with
    | "none" -> NoRetry
    | "exponential" -> ExponentialBackoff(backoffPolicyOfJson json, backoffBoundsPolicyOfJson json)
    | "fixed" -> FixedBackoff(backoffPolicyOfJson json)
    | _ -> failwithf "Unknown retry policy type: %s" type_
