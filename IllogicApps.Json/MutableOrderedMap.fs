namespace IllogicApps.Simulator

open System
open System.Collections
open System.Collections.Generic

type private DictionaryEnumerator<'K, 'V>(backingMap: Dictionary<'K, 'V>, backingList: List<'K>) =
    let mutable index = -1

    interface IDictionaryEnumerator with
        member this.Key = backingList.[index]
        member this.Value = backingMap.[backingList.[index]]

        member this.Current =
            KeyValuePair(backingList.[index], backingMap.[backingList.[index]])

        member this.Entry =
            DictionaryEntry(backingList.[index], backingMap.[backingList.[index]])

        member this.MoveNext() =
            index <- index + 1
            index < backingList.Count

        member this.Reset() = index <- -1

type MutableOrderedMap<'K, 'V> when 'K: equality() =
    let backingMap = Dictionary<'K, 'V>()
    let backingList = List<'K>()

    interface IEnumerable<KeyValuePair<'K, 'V>> with
        member this.GetEnumerator() : IEnumerator<KeyValuePair<'K, 'V>> =
            backingList
            |> Seq.map (fun key -> KeyValuePair(key, backingMap.[key]))
            |> _.GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() : IEnumerator =
            (this :> IEnumerable<KeyValuePair<'K, 'V>>).GetEnumerator()

    interface IDictionary<'K, 'V> with
        member this.Add(key, value) =
            if backingMap.ContainsKey(key) then
                raise (ArgumentException "An item with the same key has already been added.")

            backingMap.Add(key, value)
            backingList.Add(key)

        member this.Add(pair: KeyValuePair<'K, 'V>) =
            (this :> IDictionary<'K, 'V>).Add(pair.Key, pair.Value)

        member this.ContainsKey(key) = backingMap.ContainsKey(key)

        member this.Contains(pair) =
            (backingMap :> IDictionary<'K, 'V>).Contains(pair)

        member this.CopyTo(array, arrayIndex) =
            backingList
            |> Seq.indexed
            |> Seq.iter (fun (i, key) -> array.[arrayIndex + i] <- KeyValuePair(key, backingMap.[key]))

        member this.Keys = backingList.AsReadOnly()

        member this.Values =
            List(backingList |> Seq.map (fun key -> backingMap[key])).AsReadOnly()

        member this.Item
            with get key = this.[key]
            and set key value = this.[key] <- value

        member this.Count = this.Count
        member this.IsReadOnly = false

        member this.Remove(key: 'K) : bool =
            if backingMap.Remove(key) then
                backingList.Remove(key) |> ignore
                true
            else
                false

        member this.Remove(pair: KeyValuePair<'K, 'V>) : bool =
            if (backingMap :> ICollection<KeyValuePair<'K, 'V>>).Remove(pair) then
                backingList.Remove(pair.Key) |> ignore
                true
            else
                false

        member this.TryGetValue(key: 'K, value: byref<'V>) : bool = backingMap.TryGetValue(key, &value)

        member this.Clear() = this.Clear()

    interface IDictionary with
        member this.Add(key, value) =
            (this :> IDictionary<'K, 'V>).Add(key :?> 'K, value :?> 'V)

        member this.Contains(key) = backingMap.ContainsKey(key :?> 'K)

        member this.CopyTo(array, arrayIndex) =
            match array with
            | null -> failwith "array is null"
            | :? array<KeyValuePair<'K, 'V>> as array -> (this :> IDictionary<'K, 'V>).CopyTo(array, arrayIndex)
            | :? array<obj> as array ->
                backingList
                |> Seq.indexed
                |> Seq.iter (fun (i, key) -> array.[arrayIndex + i] <- KeyValuePair(key, backingMap.[key]))
            | _ -> failwith "Invalid array type"

        member this.GetEnumerator() : IDictionaryEnumerator =
            DictionaryEnumerator(backingMap, backingList)

        member this.IsFixedSize = false
        member this.IsReadOnly = false
        member this.Keys = backingList.AsReadOnly()

        member this.Values = List(backingList |> Seq.map (fun key -> this.[key])).AsReadOnly()

        member this.Item
            with get key = this.[key :?> 'K]
            and set key value = this.[key :?> 'K] <- value :?> 'V

        member this.Count = this.Count
        member this.SyncRoot = (backingMap :> IDictionary).SyncRoot
        member this.IsSynchronized = false

        member this.Remove(key) =
            (this :> IDictionary<'K, 'V>).Remove(key :?> 'K) |> ignore

        member this.Clear() = this.Clear()

    interface IReadOnlyDictionary<'K, 'V> with
        member this.ContainsKey key = this.ContainsKey key
        member this.TryGetValue(key: 'K, value: byref<'V>) : bool = backingMap.TryGetValue(key, &value)

        member this.Item
            with get key = backingMap.[key]

        member this.Count = this.Count
        member this.Keys = (this :> IDictionary<'K, 'V>).Keys
        member this.Values = (this :> IDictionary<'K, 'V>).Values

    member this.Count = backingList.Count

    member this.Item
        with get key = backingMap.[key]
        and set key value =
            if not (backingMap.ContainsKey key) then
                backingList.Add key

            backingMap.[key] <- value

    member this.ContainsKey key = backingMap.ContainsKey key

    member this.EnsureKey def key =
        match backingMap.TryGetValue key with
        | true, value -> value
        | false, _ ->
            let newValue = def key
            this.[key] <- newValue
            newValue

    member this.Clear() =
        backingList.Clear()
        backingMap.Clear()

    static member chooseValues f (map: IDictionary<'K, 'V>) =
        let next = MutableOrderedMap()

        Seq.iter
            (fun k ->
                match f k map.[k] with
                | Some v -> next.[k] <- v
                | None -> ())
            map.Keys

        next
