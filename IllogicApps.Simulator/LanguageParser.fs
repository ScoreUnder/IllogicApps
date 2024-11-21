module IllogicApps.Simulator.LanguageParser

open LanguageLexer
open IllogicApps.Json

type Ast =
    | Literal of JsonTree
    | Call of string * Ast list
    | Member of Ast * Ast
    | ForgivingMember of Ast * Ast
    | BuiltinConcat of Ast list

type private TokenOrAst =
    | Token of Token
    | Ast of Ast

let private (|LiteralIdentifier|_|) =
    function
    | "null" -> Some(Null)
    | "true" -> Some(Boolean true)
    | "false" -> Some(Boolean false)
    | _ -> None

let private collapseCall (origStack: TokenOrAst list) =
    let rec addArgs (args: Ast list) (stack: TokenOrAst list) =
        match stack with
        | Token Comma :: Ast a :: rest -> addArgs (a :: args) rest
        | Token OpenParen :: Token(Identifier name) :: rest -> Ast(Call(name, args)) :: rest
        | _ -> origStack

    match origStack with
    | Token CloseParen :: Token OpenParen :: Token(Identifier name) :: rest -> Ast(Call(name, [])) :: rest
    | Token CloseParen :: Ast a :: rest -> addArgs [ a ] rest
    | _ -> origStack

let private collapseMemberAccess (stack: TokenOrAst list) =
    match stack with
    | Token(Identifier mem) :: Token Dot :: Ast parent :: rest -> Ast(Member(parent, Literal(String mem))) :: rest
    | Token(Identifier mem) :: Token Dot :: Token QuestionMark :: Ast parent :: rest ->
        Ast(ForgivingMember(parent, Literal(String mem))) :: rest
    | _ -> stack

let private collapseIndexAccess (stack: TokenOrAst list) =
    match stack with
    | Token CloseBracket :: Ast index :: Token OpenBracket :: Ast parent :: rest -> Ast(Member(parent, index)) :: rest
    | Token CloseBracket :: Ast index :: Token OpenBracket :: Token QuestionMark :: Ast parent :: rest ->
        Ast(ForgivingMember(parent, index)) :: rest
    | _ -> stack

let parse (items: (int * Token) list list) =
    let rec parse' (items: (int * Token) list) (stack: TokenOrAst list) =
        let tryParse f stack =
            let newStack = f stack
            if newStack = stack then None else Some newStack

        let must f stack =
            let newStack = f stack

            if newStack = stack then
                failwithf "Unexpected token %O" (snd (List.head items))
            else
                newStack

        match items with
        | [] ->
            match stack with
            | [ Ast a ] -> a
            | _ -> failwithf "Unexpected end of input. Dump: %O" (List.rev stack)
        | (pos, token) :: rest ->
            try
                match token with
                | Identifier(LiteralIdentifier v) ->
                    Token token :: stack
                    |> tryParse collapseMemberAccess
                    |> Option.defaultValue (Ast(Literal(v)) :: stack)
                | LanguageLexer.String str -> Ast(Literal(String str)) :: stack
                | LanguageLexer.Integer num -> Ast(Literal(Integer num)) :: stack
                | Number num -> Ast(Literal(Float num)) :: stack
                | CloseParen -> Token token :: stack |> must collapseCall
                | CloseBracket -> Token token :: stack |> must collapseIndexAccess
                | _ -> Token token :: stack |> collapseMemberAccess
            with ex ->
                raise
                <| new System.Exception(
                    sprintf "Error at position %d: %s. Dump: %O" pos ex.Message (List.rev stack),
                    ex
                )
            |> parse' rest

    let result = items |> List.map (fun items -> parse' items [])

    match result with
    | [ a ] -> a
    | l -> BuiltinConcat l
