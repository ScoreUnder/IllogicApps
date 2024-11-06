namespace IllogicApps.Json

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Immutable
open Microsoft.FSharp.Collections

type OrderedMap<[<ComparisonConditionalOn>] 'K, [<ComparisonConditionalOn>] 'V> when 'K: equality and 'V: equality
    private (backingMap: ImmutableDictionary<'K, 'V>, backingArray: ImmutableArray<'K>) =
    let values: 'V ImmutableArray Lazy =
        lazy
            backingArray
            |> Seq.map (fun key -> backingMap.[key])
            |> ImmutableArray.CreateRange

    static member CreateRange(keyValuePairs: KeyValuePair<'K, 'V> seq) =
        let backingMap, backingArray =
            let cachedSeq = Seq.toArray keyValuePairs
            let asArray = cachedSeq |> Array.map _.Key |> ImmutableArray.CreateRange
            let asMap = ImmutableDictionary.CreateRange(cachedSeq)
            asMap, asArray

        OrderedMap(backingMap, backingArray)

    static member internal CreateUnsafe(backingMap, backingArray) = OrderedMap(backingMap, backingArray)

    member public this.Keys = backingArray
    member inline public this.Count = this.Keys.Length
    member private this.BackingMap = backingMap
    member this.Values = values.Value

    member this.Item
        with get key = backingMap.[key]

    member this.ContainsKey key = backingMap.ContainsKey key
    member this.TryGetValue(key, value: 'V outref) = backingMap.TryGetValue(key, &value)

    interface IEnumerable with
        member this.GetEnumerator() : IEnumerator =
            (this :> IEnumerable<KeyValuePair<'K, 'V>>).GetEnumerator()

    interface IEnumerable<KeyValuePair<'K, 'V>> with
        member this.GetEnumerator() =
            backingArray
            |> Seq.map (fun key -> KeyValuePair(key, backingMap.[key]))
            |> _.GetEnumerator()

    interface IReadOnlyCollection<KeyValuePair<'K, 'V>> with
        member this.Count = this.Count

    interface IReadOnlyDictionary<'K, 'V> with
        member this.Keys = this.Keys
        member this.Values = this.Values
        member this.TryGetValue(key, value) = this.TryGetValue(key, &value)

        member this.Item
            with get key = this.[key]

        member this.ContainsKey key = this.ContainsKey key

    // ICollection to speed up Seq.foldBack and other such operations
    // which might require a copyTo operation
    interface ICollection<KeyValuePair<'K, 'V>> with
        member this.Add _ = failwith "Not supported"
        member this.Clear() = failwith "Not supported"

        member this.Contains(KeyValue(key, value)) =
            this.ContainsKey key && this.[key] = value

        member this.CopyTo(array, arrayIndex) =
            backingArray
            |> Seq.iteri (fun i key -> array.[arrayIndex + i] <- KeyValuePair(key, backingMap.[key]))

        member this.Remove _ = failwith "Not supported"
        member this.Count = this.Count
        member this.IsReadOnly = true

    interface IReadOnlyList<KeyValuePair<'K, 'V>> with
        member this.Item
            with get index = KeyValuePair(backingArray.[index], backingMap.[backingArray.[index]])

    interface IStructuralEquatable with
        member this.Equals(other, comparer) =
            match other with
            | :? OrderedMap<'K, 'V> as other ->
                if (this.Keys :> IStructuralEquatable).Equals(other.Keys, comparer) then
                    // BackingMap is an ImmutableDictionary, which does not implement IStructuralEquatable.
                    // At this point we know that the keys are equal, so we can use that information to accurately
                    // compare the backing maps.
                    Seq.forall (fun key -> comparer.Equals(this.BackingMap.[key], other.BackingMap.[key])) this.Keys
                else
                    false
            | _ -> false

        member this.GetHashCode(comparer) =
            173 * comparer.GetHashCode(this.Keys) + comparer.GetHashCode(this.BackingMap)

    interface IEquatable<OrderedMap<'K, 'V>> with
        member this.Equals(other) =
            obj.ReferenceEquals(this, other)
            || (this :> IStructuralEquatable).Equals(other, EqualityComparer.Default)

    override this.Equals(other) =
        obj.ReferenceEquals(this, other)
        || match other with
           | :? OrderedMap<'K, 'V> as other -> (this :> IEquatable<OrderedMap<'K, 'V>>).Equals(other)
           | _ -> false

    override this.GetHashCode() =
        (this :> IStructuralEquatable).GetHashCode(EqualityComparer.Default)

    interface IStructuralComparable with
        member this.CompareTo(other, comparer) =
            match other with
            | :? OrderedMap<'K, 'V> as other -> this.CompareTo other comparer
            | _ -> 1

    interface IComparable with
        member this.CompareTo(other) =
            (this :> IStructuralComparable).CompareTo(other, Comparer.Default)

    interface IComparable<OrderedMap<'K, 'V>> with
        member this.CompareTo(other) = this.CompareTo other Comparer.Default

    member internal this.CompareTo (other: OrderedMap<'K, 'V>) (comparer: IComparer) =
        let keysComparison =
            (this.Keys :> IStructuralComparable).CompareTo(other.Keys, comparer)

        if keysComparison <> 0 then
            keysComparison
        else
            // BackingMap is an ImmutableDictionary, which does not implement IStructuralComparable.
            // As with the Equals method, we can use the fact that the keys are equal to accurately compare the backing maps.
            Seq.tryPick
                (fun key ->
                    let valueComparison =
                        comparer.Compare(this.BackingMap.[key], other.BackingMap.[key])

                    if valueComparison <> 0 then Some valueComparison else None)
                this.Keys
            |> Option.defaultValue 0

module OrderedMap =
    type Builder<'K, 'V when 'K: equality and 'V: equality>() =
        let mutable backingMap = ImmutableDictionary.CreateBuilder<'K, 'V>()
        let mutable backingArray = ImmutableArray.CreateBuilder<'K>()

        member this.Add(key: 'K, value: 'V) =
            backingMap.Add(key, value)
            backingArray.Add key
            this

        member this.Build() =
            OrderedMap.CreateUnsafe<'K, 'V>(backingMap.ToImmutable(), backingArray.DrainToImmutable())

    let empty<'K, 'V when 'K: equality and 'V: equality> =
        OrderedMap<'K, 'V>.CreateRange([])

    let inline isEmpty (m: OrderedMap<'a, 'b>) = m.Count = 0

    let ofSeq seq =
        seq |> Seq.map (fun (k, v) -> KeyValuePair(k, v)) |> OrderedMap.CreateRange

    let inline ofList list = ofSeq list
    let inline ofArray array = ofSeq array
    let inline ofMap (map: Map<'K, 'V>) = OrderedMap.CreateRange(map)


    let fold (f: 'State -> 'K -> 'V -> 'State) (state: 'State) (m: OrderedMap<'K, 'V>) : 'State =
        Seq.fold (fun acc (KeyValue(k, v)) -> f acc k v) state m

    let foldBack (f: 'K -> 'V -> 'State -> 'State) (m: OrderedMap<'K, 'V>) (state: 'State) : 'State =
        Seq.foldBack (fun (KeyValue(k, v)) -> f k v) m state

    let set (key: 'K) (value: 'V) (m: OrderedMap<'K, 'V>) =
        if m.ContainsKey key then
            OrderedMap.CreateRange(
                m
                |> Seq.map (fun (KeyValue(k, v)) -> KeyValuePair(k, (if k = key then value else v)))
            )
        else
            OrderedMap.CreateRange(m |> Seq.append [ KeyValuePair(key, value) ])

    let toSeq (m: OrderedMap<'K, 'V>) =
        m |> Seq.map (fun (KeyValue(k, v)) -> k, v)

    let toList (m: OrderedMap<'K, 'V>) =
        foldBack (fun k v acc -> (k, v) :: acc) m []

    let toArray (m: OrderedMap<'K, 'V>) = Seq.toArray m

    let toMap (m: OrderedMap<'K, 'V>) = toSeq m |> Map.ofSeq

    let tryFind (key: 'K) (m: OrderedMap<'K, 'V>) =
        match m.TryGetValue key with
        | true, v -> Some v
        | _ -> None

    let tryPick (f: 'K -> 'V -> 'State option) (m: OrderedMap<'K, 'V>) : 'State option =
        Seq.tryPick (fun (KeyValue(k, v)) -> f k v) m

    let find (key: 'K) (m: OrderedMap<'K, 'V>) = m.[key]

    let pick (f: 'K -> 'V -> 'State option) (m: OrderedMap<'K, 'V>) : 'State =
        Seq.pick (fun (KeyValue(k, v)) -> f k v) m

    let exists (f: 'K -> 'V -> bool) (m: OrderedMap<'K, 'V>) =
        Seq.exists (fun (KeyValue(k, v)) -> f k v) m

    let forall (f: 'K -> 'V -> bool) (m: OrderedMap<'K, 'V>) =
        Seq.forall (fun (KeyValue(k, v)) -> f k v) m

    let map (f: 'K -> 'V -> 'K * 'V) (m: OrderedMap<'K, 'V>) =
        Seq.map (fun (KeyValue(k, v)) -> let k', v' = f k v in KeyValuePair(k', v')) m
        |> OrderedMap.CreateRange

    let forgivingMap (f: 'K -> 'V -> ('K * 'V) option) (m: OrderedMap<'K, 'V>) =
        foldBack
            (fun k v (lst, set) ->
                match f k v with
                | Some(k', v') ->
                    if Set.contains k' set then
                        lst, set
                    else
                        (KeyValuePair(k', v') :: lst), Set.add k' set
                | None -> lst, set)
            m
            ([], Set.empty)
        |> fst
        |> OrderedMap.CreateRange

    let collect (f: 'K -> 'V -> ('K * 'V) seq) (m: OrderedMap<'K, 'V>) =
        m
        |> Seq.collect (fun (KeyValue(k, v)) -> f k v |> Seq.map (fun (k', v') -> KeyValuePair(k', v')))
        |> OrderedMap.CreateRange

    let forgivingCollect (f: 'K -> 'V -> ('K * 'V) seq) (m: OrderedMap<'K, 'V>) =
        m
        |> Seq.collect (fun (KeyValue(k, v)) -> f k v |> Seq.map (fun (k', v') -> KeyValuePair(k', v')))
        |> (fun state ->
            Seq.foldBack
                (fun (KeyValue(k, _) as kvp) (lst, set) ->
                    if Set.contains k set then
                        lst, set
                    else
                        (kvp :: lst), Set.add k set)
                state
                ([], Set.empty))
        |> fst
        |> OrderedMap.CreateRange

    let filter (f: 'K -> 'V -> bool) (m: OrderedMap<'K, 'V>) =
        Seq.filter (fun (KeyValue(k, v)) -> f k v) m |> OrderedMap.CreateRange

    let partition (f: 'K -> 'V -> bool) (m: OrderedMap<'K, 'V>) =
        let yes, no =
            fold (fun (yes, no) k v -> if f k v then ((k, v) :: yes), no else yes, ((k, v) :: no)) ([], []) m

        ofList (List.rev yes), ofList (List.rev no)

    let iter (f: 'K -> 'V -> unit) (m: OrderedMap<'K, 'V>) =
        Seq.iter (fun (KeyValue(k, v)) -> f k v) m
