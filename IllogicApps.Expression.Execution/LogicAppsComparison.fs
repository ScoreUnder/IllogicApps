module IllogicApps.Expression.Execution.LogicAppsComparison

open IllogicApps.Json

type ComparisonResult =
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

let checkComparison strictOrdering results a b =
    let comparison = compareValues a b

    if strictOrdering && comparisonIsUnordered comparison then
        failwithf "Expected %O to be strictly comparable to %O" a b

    List.contains comparison results
