module IllogicApps.Extra.ExpressionInfixer.InfixExpressionDumper

open System.IO
open IllogicApps.Extra.ExpressionInfixer.Operators
open IllogicApps.Extra.ExpressionInfixer.InfixExpression
open IllogicApps.Extra.ExpressionInfixer.InfixExpressionConverter
open IllogicApps.Json

let dumpInfixAst (out: TextWriter) (ast: InfixAst) : unit =
    let rec aux precedence associativity =
        function
        | Literal l -> out.Write(Conversions.stringOfJson l)
        | Call(name, args) ->
            out.Write(name)
            out.Write("(")
            let prec = getPrecedence opComma
            let assoc = getAssociativity opComma

            List.iteri
                (fun i v ->
                    (if i > 0 then
                         out.Write(", "))

                    aux prec assoc v)
                args

            out.Write(")")
        | Operator(args, op) ->
            let maybeBracket prec assoc f =
                let needsBracketing =
                    prec > precedence
                    || prec = precedence && (assoc <> associativity || assoc = NonAssociative)

                if needsBracketing then
                    out.Write("(")

                f ()

                if needsBracketing then
                    out.Write(")")

            let getAssocIndex =
                function
                | Left -> 0
                | Right -> List.length args - 1
                | NonAssociative -> -1

            match op with
            | Unary(name, prec, assoc) ->
                maybeBracket prec assoc
                <| fun () ->
                    let arg = List.head args

                    match assoc with
                    | Left ->
                        aux prec assoc arg
                        out.Write(name)
                    | Right ->
                        out.Write(name)
                        aux prec assoc arg
                    | NonAssociative -> failwith "internal error: nonassociative unary operator"
            | Binary(name, prec, assoc) ->
                let assocIndex = getAssocIndex assoc

                maybeBracket prec assoc
                <| fun () ->
                    List.iteri
                        (fun i v ->
                            if i > 0 then
                                out.Write(" ")
                                out.Write(name)
                                out.Write(" ")

                            // Force non-associativity if we're not on the correct side
                            let assoc = if i = assocIndex then assoc else NonAssociative
                            aux prec assoc v)
                        args
            | BinaryIdentifier(name, prec, assoc) ->
                maybeBracket prec assoc
                <| fun () ->
                    match args, assoc with
                    | [ arg1; Literal(String arg2) ], Left ->
                        aux prec assoc arg1
                        out.Write(name)
                        out.Write(arg2)
                    | [ Literal(String arg1); arg2 ], Right ->
                        out.Write(arg1)
                        out.Write(name)
                        aux prec assoc arg2
                    | _ -> failwith "internal error: poorly formed BinaryIdentifier"
            | BinaryBracketed(nameOpen, nameClose, prec, assoc) ->
                maybeBracket prec assoc
                <| fun () ->
                    match args with
                    | [ arg1; arg2 ] ->
                        match assoc with
                        | Left ->
                            aux prec assoc arg1
                            out.Write(nameOpen)
                            aux precedenceBracketed NonAssociative arg2
                            out.Write(nameClose)
                        | Right ->
                            out.Write(nameOpen)
                            aux precedenceBracketed NonAssociative arg1
                            out.Write(nameClose)
                            aux prec assoc arg2
                        | NonAssociative -> failwith "internal error: nonassociative binary bracketed operator"
                    | _ -> failwith "expected exactly two arguments"
            | Ternary(name1, name2, prec, assoc) ->
                maybeBracket prec assoc
                <| fun () ->
                    match args with
                    | [ arg1; arg2; arg3 ] ->
                        aux prec (if assoc = Left then Left else NonAssociative) arg1
                        out.Write(" ")
                        out.Write(name1)
                        out.Write(" ")
                        aux prec NonAssociative arg2
                        out.Write(" ")
                        out.Write(name2)
                        out.Write(" ")
                        aux prec (if assoc = Right then Right else NonAssociative) arg3
                    | _ -> failwith "expected exactly three arguments"

    aux precedenceBracketed NonAssociative ast

let dumpAsInfix out ast =
    ast |> astToInfixAst |> collapseInfixAst |> dumpInfixAst out
