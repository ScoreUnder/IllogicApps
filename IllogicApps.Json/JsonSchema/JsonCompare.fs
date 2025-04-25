namespace IllogicApps.Json

open System.Collections.Generic
open Conversions

module JsonCompare =
    // 4.2.2 Instance Equality
    let rec compareJsons (a: JsonTree) (b: JsonTree) : int =
        match a, b with
        | JsonTree.Null, JsonTree.Null -> 0
        | JsonTree.Boolean a, JsonTree.Boolean b -> compare a b
        | JsonTree.String a, JsonTree.String b -> compare a b
        | NumbersAsInteger(a, b) -> compare a b
        | NumbersAsFloat(a, b) -> compare a b
        | NumbersAsDecimal(a, b) -> compare a b
        | JsonTree.Array a, JsonTree.Array b ->
            let lengthCompare = compare a.Length b.Length

            if lengthCompare <> 0 then
                lengthCompare
            else
                Seq.map2 compareJsons a b
                |> Seq.tryFind (fun x -> x <> 0)
                |> Option.defaultValue 0
        | JsonTree.Object a, JsonTree.Object b ->
            let countCompare = compare a.Count b.Count

            if countCompare <> 0 then
                countCompare
            else
                let keysCompare =
                    Seq.map2 compare (Seq.sort a.Keys) (Seq.sort b.Keys)
                    |> Seq.tryFind (fun x -> x <> 0)
                    |> Option.defaultValue 0

                if keysCompare <> 0 then
                    keysCompare
                else
                    Seq.tryPick (fun k1 -> let c = compareJsons a.[k1] b.[k1] in if c <> 0 then Some c else None) a.Keys
                    |> Option.defaultValue 0
        | _ -> compare (JsonTree.getType a) (JsonTree.getType b)

    let jsonsEqual (a: JsonTree) (b: JsonTree) = compareJsons a b = 0

    let typesMatch (schemaType: SchemaType) (json: JsonTree) =
        match schemaType, json with
        | SchemaType.Null, Null -> true
        | SchemaType.Boolean, Boolean _ -> true
        | SchemaType.Object, Object _ -> true
        | SchemaType.Array, Array _ -> true
        | SchemaType.Number, Integer _ -> true
        | SchemaType.Number, Float _ -> true
        | SchemaType.Number, Decimal _ -> true
        | SchemaType.Integer, Integer _ -> true
        | SchemaType.Integer, Float f when System.Double.IsInteger(f) -> true
        | SchemaType.Integer, Decimal d when System.Decimal.Truncate(d) = d -> true
        | SchemaType.String, String _ -> true
        | _ -> false

type UnorderedJsonComparer() =
    interface IComparer<JsonTree> with
        member _.Compare(x, y) = JsonCompare.compareJsons x y

    interface IEqualityComparer<JsonTree> with
        member _.Equals(x, y) = JsonCompare.jsonsEqual x y

        member _.GetHashCode(x) =
            let typePrime = 28415693

            match x with
            | Null -> 0
            | Boolean b -> typePrime ^^^ b.GetHashCode()
            | String s -> typePrime * 2 ^^^ s.GetHashCode()
            | Integer i -> typePrime * 3 ^^^ i.GetHashCode()
            | Decimal d ->
                if
                    System.Decimal.Truncate d = d
                    && d <= decimal System.Int64.MaxValue
                    && d >= decimal System.Int64.MinValue
                then
                    typePrime * 3 ^^^ (int64 d).GetHashCode()
                else
                    typePrime * 4 ^^^ d.GetHashCode()
            | Float f ->
                if System.Double.IsNaN f then
                    typePrime * 5 ^^^ 0
                else if System.Double.IsNegativeInfinity f then
                    typePrime * 5 ^^^ 1
                else if System.Double.IsPositiveInfinity f then
                    typePrime * 5 ^^^ 2
                else if
                    System.Double.IsInteger f
                    && f <= float System.Int64.MaxValue
                    && f >= float System.Int64.MinValue
                then
                    typePrime * 3 ^^^ (int64 f).GetHashCode()
                else if f <= float System.Decimal.MaxValue && f >= float System.Decimal.MinValue then
                    typePrime * 4 ^^^ (decimal f).GetHashCode()
                else
                    typePrime * 6 ^^^ f.GetHashCode()
            | Array a ->
                typePrime * 7
                ^^^ (PerfSeq.fold (fun acc item -> 25728533 * acc ^^^ item.GetHashCode()) 0 a)
            | Object o ->
                let sortedKeys = Seq.sort o.Keys

                typePrime * 8
                ^^^ (PerfSeq.fold (fun acc k -> 36513959 * acc ^^^ k.GetHashCode() ^^^ o.[k].GetHashCode()) 0 sortedKeys)
