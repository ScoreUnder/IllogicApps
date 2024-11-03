module IllogicApps.Simulator.BuiltinCondition

open IllogicApps.Json

let private twoArg (f: JsonTree -> JsonTree -> bool) (args: JsonTree list) : bool =
    match args with
    | [ a; b ] -> f a b
    | _ -> failwithf "Expected 2 arguments, got %d" args.Length

type private ComparisonResult =
    | Equal // The two values are equal
    | UnorderedEqual // The two values are equal, but not ordered (i.e. you can't say that true is greater than false)
    | Greater // The first value is greater than the second
    | Less // The first value is less than the second
    | SneakyIncomparable // NaN is involved (pretends to be ordered, but is actually incomparable)
    | Incomparable // The two values are incomparable (i.e. not equal and not ordered)

let private comparisonIsUnordered =
    function
    | UnorderedEqual
    | Incomparable -> true
    | _ -> false

let private compareStrings a b =
    match System.String.CompareOrdinal(a, b) with
    | 0 -> Equal
    | n when n > 0 -> Greater
    | _ -> Less

let private compareIntegers a b =
    if a = b then Equal
    elif a > b then Greater
    else Less

let private compareFloats a b =
    if System.Double.IsNaN(a) || System.Double.IsNaN(b) then
        SneakyIncomparable
    elif a = b then
        Equal
    elif a > b then
        Greater
    else
        Less

let private compareDecimals a b =
    if a = b then Equal
    elif a > b then Greater
    else Less

let private compareValues (a: JsonTree) (b: JsonTree) : ComparisonResult =
    match a, b with
    | String a, String b -> compareStrings a b
    | Boolean a, Boolean b when a = b -> UnorderedEqual
    | String s, Boolean _ when s = Conversions.stringOfJson b -> UnorderedEqual
    | Boolean _, String s when s = Conversions.stringOfJson a -> UnorderedEqual
    | Null, Null -> UnorderedEqual
    | Conversions.NumbersAsInteger(a, b) -> compareIntegers a b
    | Conversions.NumbersAsFloat(a, b) -> compareFloats a b
    | Conversions.NumbersAsDecimal(a, b) -> compareDecimals a b
    | _ -> Incomparable

let private checkComparison strictOrdering results a b =
    let comparison = compareValues a b

    if strictOrdering && comparisonIsUnordered comparison then
        failwithf "Expected %A to be strictly comparable to %A" a b

    List.contains comparison results

type LanguageCondition = JsonTree list -> bool

let condContains =
    twoArg (fun a b ->
        match a, b with
        | Array arr, _ -> arr.Contains(b)
        | Object o, String k -> o.ContainsKey(k)
        | String a, String b -> a.Contains(b)
        | _ ->
            failwithf
                "Expected array (and element), object (and string key), or string (and substring), got %A and %A"
                (JsonTree.getType a)
                (JsonTree.getType b))

let condEquals: LanguageCondition =
    twoArg (checkComparison false [ Equal; UnorderedEqual ])

let condGreater: LanguageCondition = twoArg (checkComparison true [ Greater ])

let condGreaterOrEquals: LanguageCondition =
    twoArg (checkComparison true [ Greater; Equal ])

let condLess: LanguageCondition = twoArg (checkComparison true [ Less ])

let condLessOrEquals: LanguageCondition =
    twoArg (checkComparison true [ Less; Equal ])

let condStartsWith: LanguageCondition =
    twoArg (fun a b ->
        let a = Conversions.ensureString a
        let b = Conversions.ensureString b
        a.StartsWith(b))

let condEndsWith: LanguageCondition =
    twoArg (fun a b ->
        let a = Conversions.ensureString a
        let b = Conversions.ensureString b
        a.EndsWith(b))

let conditions: Map<string, LanguageCondition> =
    Map.ofList
        [ "contains", condContains
          "equals", condEquals
          "greater", condGreater
          "greaterOrEquals", condGreaterOrEquals
          "less", condLess
          "lessOrEquals", condLessOrEquals
          "startsWith", condStartsWith
          "endsWith", condEndsWith ]

let (|LanguageCondition|_|) name =
    match conditions.TryGetValue(name) with
    | true, f -> Some f
    | _ -> None
