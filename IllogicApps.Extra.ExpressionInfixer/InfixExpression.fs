module IllogicApps.Extra.ExpressionInfixer.InfixExpression

open IllogicApps.Extra.ExpressionInfixer.Operators
open IllogicApps.Json

type InfixAst =
    | Operator of InfixAst list * Operator
    | Call of string * InfixAst list
    | Literal of JsonTree
