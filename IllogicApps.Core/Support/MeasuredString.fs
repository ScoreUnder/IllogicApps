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

    let inline toByteArray ([<InlineIfLambda>] f: string -> byte array) (str: string<'m>) : byte<'m> array =
        str |> unmark<'m, 1> |> f |> Unchecked.unbox<byte<'m> array>

    let inline ofByteArray ([<InlineIfLambda>] f: byte array -> string) (bytes: byte<'m> array) : string<'m> =
        bytes |> Unchecked.unbox<byte array> |> f |> mark<'m, 1>
