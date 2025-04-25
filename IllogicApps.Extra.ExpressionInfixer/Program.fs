module IllogicApps.Extra.ExpressionInfixer.Program

open IllogicApps.Json
module ExpressionLexer = IllogicApps.Expression.Parsing.Lexer
module ExpressionParser = IllogicApps.Expression.Parsing.Parser

[<EntryPoint>]
let main args =
    ignore args

    printfn "Enter an expression to parse:"
    let input = System.Console.ReadLine()

    // Attempt to parse input as a JSON string first, because that's easier to copy-paste
    let input =
        try
            JsonParser.parse input |> Conversions.ensureString
        with _ ->
            input

    let parsed = input |> ExpressionLexer.lex |> ExpressionParser.parse

    printfn "Dump:"
    ExpressionDumper.dumpExpression System.Console.Out parsed

    printfn "\n\nInfix:"
    InfixExpressionDumper.dumpAsInfix System.Console.Out parsed

    printfn "\n\nBack-conversion:"

    parsed
    |> InfixExpressionConverter.astToInfixAst
    |> InfixExpressionConverter.collapseInfixAst
    |> InfixExpressionConverter.infixAstToAst
    |> ExpressionDumper.dumpExpression System.Console.Out

    printfn ""

    0
