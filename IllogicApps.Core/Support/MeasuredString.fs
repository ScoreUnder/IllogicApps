namespace IllogicApps.Core.Support

[<MeasureAnnotatedAbbreviation>]
type string<[<Measure>] 'a> = string

module MeasuredString =
    [<RequiresExplicitTypeArguments>]
    let inline mark<[<Measure>] 'm, [<Measure>] 'o> (str: string<'o>) : string<'o * 'm> =
        Unchecked.unbox<string<'o * 'm>> str

    [<RequiresExplicitTypeArguments>]
    let inline unmark<[<Measure>] 'm, [<Measure>] 'o> (str: string<'o * 'm>) : string<'o> =
        Unchecked.unbox<string<'o>> str
