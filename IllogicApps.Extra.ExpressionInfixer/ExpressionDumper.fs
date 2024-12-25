module IllogicApps.Extra.ExpressionInfixer.ExpressionDumper

open System.IO
open IllogicApps.Expression.Parsing.Parser
open IllogicApps.Json

let rec dumpExpression (out: TextWriter) (expr: Ast) =
    match expr with
    | Literal l ->
        match l with
        | String s ->
            out.Write("'")
            out.Write(s.Replace("'", "''"))
            out.Write("'")
        | Integer i -> out.Write(string i)
        | Float f -> out.Write(string f)
        | Boolean b -> out.Write(if b then "true" else "false")
        | Null -> out.Write("null")
        | _ -> failwithf "Literal has unexpected type: %O on literal %O" (JsonTree.getType l) l
    | Call(name, args) ->
        out.Write(name)
        out.Write("(")

        List.iteri
            (fun i v ->
                if i > 0 then
                    out.Write(",")

                dumpExpression out v)
            args

        out.Write(")")
    | Member(parent, name) ->
        dumpExpression out parent
        out.Write("[")
        dumpExpression out name
        out.Write("]")
    | ForgivingMember(parent, name) ->
        dumpExpression out parent
        out.Write("?[")
        dumpExpression out name
        out.Write("]")
    | BuiltinConcat(args) ->
        List.iter
            (fun v ->
                match v with
                | Literal(String s) when not (s.StartsWith("@")) -> out.Write(s)
                | arg ->
                    out.Write("@{")
                    dumpExpression out arg
                    out.Write("}"))
            args
