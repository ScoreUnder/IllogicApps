module IllogicApps.Extra.ExpressionInfixer.InfixExpressionConverter

open IllogicApps.Expression.Parsing.Parser
open IllogicApps.Extra.ExpressionInfixer.Operators
open IllogicApps.Extra.ExpressionInfixer.InfixExpression
open IllogicApps.Json

type private I = InfixAst
type private A = Ast

let private safeNameRegex =
    System.Text.RegularExpressions.Regex("^[a-zA-Z_][a-zA-Z0-9_]*$")

let rec astToInfixAst (ast: A) : I =
    match ast with
    | A.Literal l -> I.Literal l
    | A.Call(name, args) when functionToOperator.ContainsKey(name) ->
        let operator = functionToOperator.[name]

        match operator with
        | Unary _ ->
            if List.length args <> 1 then
                failwithf "expected exactly one argument for function %s" name
        | Binary _ ->
            // Unlike the other operator types, Binary operators can have extra arguments which
            // correspond to chained invocations. So "1 && 2 && 3" is one Binary operator with three arguments.
            if List.length args < 2 then
                failwithf "expected at least two arguments for function %s" name
        | BinaryIdentifier _ ->
            if List.length args <> 2 then
                failwithf "expected exactly two arguments for function %s" name
        | BinaryBracketed _ ->
            if List.length args <> 2 then
                failwithf "expected exactly two arguments for function %s" name
        | Ternary _ ->
            if List.length args <> 3 then
                failwithf "expected exactly three arguments for function %s" name

        I.Operator(List.map astToInfixAst args, operator)
    | A.Call(name, args) -> I.Call(name, List.map astToInfixAst args)
    | A.Member(parent, A.Literal(String name as jName)) when safeNameRegex.IsMatch(name) ->
        I.Operator([ astToInfixAst parent; I.Literal jName ], opMembership)
    | A.Member(parent, name) -> I.Operator([ astToInfixAst parent; astToInfixAst name ], opMembershipBracketed)
    | A.ForgivingMember(parent, A.Literal(String name as jName)) when safeNameRegex.IsMatch(name) ->
        I.Operator([ astToInfixAst parent; I.Literal jName ], opMembershipForgiving)
    | A.ForgivingMember(parent, name) ->
        I.Operator([ astToInfixAst parent; astToInfixAst name ], opMembershipForgivingBracketed)
    | A.BuiltinConcat(args) ->
        if List.isEmpty args then
            I.Literal(String "")
        else
            let args =
                match args with
                | [ arg ] -> [ A.Literal(String ""); arg ]
                | _ -> args

            I.Operator(List.map astToInfixAst args, opConcat)

let rec collapseInfixAst =
    function
    | Operator([ Operator(args, opInner) ], opOuter) when opInner = opEquals && opOuter = opNot ->
        collapseInfixAst (Operator(args, opNotEquals))
    | Operator(Operator(argsInner, opInner) :: argsOuter, opOuter) when
        opInner = opOuter && Set.contains opOuter internallyChainableOperators
        ->
        collapseInfixAst (Operator(argsInner @ argsOuter, opOuter))
    // Non-collapsible cases:
    | Operator(args, op) -> Operator(List.map collapseInfixAst args, op)
    | Call(name, args) -> Call(name, List.map collapseInfixAst args)
    | Literal l -> Literal l

let infixAstToAst (ast: I) : A =
    let rec aux ast =
        match ast with
        | I.Literal l -> A.Literal l
        | I.Call(name, args) -> A.Call(name, List.map aux args)
        | I.Operator(args, op) ->
            match Map.tryFind op operatorToFunction with
            | Some name -> A.Call(name, List.map aux args)
            | None ->
                // Special-cased operators...
                match op with
                | BinaryIdentifier(".", _, _)
                | BinaryBracketed("[", "]", _, _) ->
                    let lhs, rhs =
                        match args with
                        | [ lhs; rhs ] -> lhs, rhs
                        | _ -> failwith "expected exactly two arguments for membership operator"

                    A.Member(aux lhs, aux rhs)
                | BinaryIdentifier("?.", _, _)
                | BinaryBracketed("?[", "]", _, _) ->
                    let lhs, rhs =
                        match args with
                        | [ lhs; rhs ] -> lhs, rhs
                        | _ -> failwith "expected exactly two arguments for forgiving membership operator"

                    A.ForgivingMember(aux lhs, aux rhs)
                | Binary("!=", _, _) ->
                    let lhs, rhs =
                        match args with
                        | [ lhs; rhs ] -> lhs, rhs
                        | _ -> failwith "expected exactly two arguments for not equals operator"

                    A.Call("not", [ A.Call("equals", [ aux lhs; aux rhs ]) ])
                | _ -> failwithf "Not sure what to do with operator %O" op

    match ast with
    | I.Operator(args, op) when op = opConcat -> A.BuiltinConcat(List.map aux args)
    | _ -> aux ast
