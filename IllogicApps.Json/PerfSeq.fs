/// Performant sequence operations
/// i.e. inline operations and such
module internal IllogicApps.Json.PerfSeq

open System.Text

let inline fold (folder: 'State -> 'Elem -> 'State) (state: 'State) (seq: 'Elem seq) =
    let mutable acc = state
    let iter = seq.GetEnumerator()

    while iter.MoveNext() do
        acc <- folder acc iter.Current

    acc

let inline foldi (folder: int -> 'State -> 'Elem -> 'State) (state: 'State) (seq: 'Elem seq) =
    let mutable acc = state
    let mutable i = 0
    let iter = seq.GetEnumerator()

    while iter.MoveNext() do
        acc <- folder i acc iter.Current
        i <- i + 1

    acc

let inline partitionRev ([<InlineIfLambda>] f: 'a -> bool) (seq: 'a seq) : 'a list * 'a list =
    let mutable trueList = []
    let mutable falseList = []
    let iter = seq.GetEnumerator()

    while iter.MoveNext() do
        let x = iter.Current

        if f x then
            trueList <- x :: trueList
        else
            falseList <- x :: falseList

    trueList, falseList

let inline forall ([<InlineIfLambda>] f: 'a -> bool) (seq: 'a seq) : bool =
    let iter = seq.GetEnumerator()

    let rec forallLoop () =
        if iter.MoveNext() then
            if f iter.Current then forallLoop () else false
        else
            true

    forallLoop ()

let inline exists ([<InlineIfLambda>] f: 'a -> bool) (seq: 'a seq) : bool =
    let iter = seq.GetEnumerator()

    let rec existsLoop () =
        if iter.MoveNext() then
            if f iter.Current then true else existsLoop ()
        else
            false

    existsLoop ()

let join (start: string) (sep: string) (finish: string) (subject: 'a seq) : string =
    let builder = StringBuilder(64)

    builder.Append(start) |> ignore
    let iter = subject.GetEnumerator()

    if iter.MoveNext() then
        builder.Append(iter.Current) |> ignore

        while iter.MoveNext() do
            builder.Append(sep) |> ignore
            builder.Append(iter.Current) |> ignore

    builder.Append(finish) |> ignore
    builder.ToString()
