module IllogicApps.Extra.ExpressionInfixer.Operators

type Associativity =
    | Left
    | Right
    | NonAssociative

type Operator =
    | Unary of string * int * Associativity
    | Binary of string * int * Associativity
    | BinaryIdentifier of string * int * Associativity
    | BinaryBracketed of string * string * int * Associativity
    | Ternary of string * string * int * Associativity

let getPrecedence =
    function
    | Unary(_, prec, _) -> prec
    | Binary(_, prec, _) -> prec
    | BinaryIdentifier(_, prec, _) -> prec
    | BinaryBracketed(_, _, prec, _) -> prec
    | Ternary(_, _, prec, _) -> prec

let getAssociativity =
    function
    | Unary(_, _, assoc) -> assoc
    | Binary(_, _, assoc) -> assoc
    | BinaryIdentifier(_, _, assoc) -> assoc
    | BinaryBracketed(_, _, _, assoc) -> assoc
    | Ternary(_, _, _, assoc) -> assoc

let getOpStr =
    function
    | Unary(name, _, _) -> name
    | Binary(name, _, _) -> name
    | BinaryIdentifier(name, _, _) -> name
    | BinaryBracketed(name, _, _, _) -> name
    | Ternary(name, _, _, _) -> name

let getOpSecondaryStr =
    function
    | Unary(name, _, _) -> name
    | Binary(name, _, _) -> name
    | BinaryIdentifier(name, _, _) -> name
    | BinaryBracketed(_, name, _, _) -> name
    | Ternary(_, name, _, _) -> name

let opMembership = BinaryIdentifier(".", 0, Left)
let opMembershipForgiving = BinaryIdentifier("?.", 0, Left)
let opMembershipBracketed = BinaryBracketed("[", "]", 0, Left)
let opMembershipForgivingBracketed = BinaryBracketed("?[", "]", 0, Left)
let opNot = Unary("!", 1, Right)
let opNotEquals = Binary("!=", 5, Left)
let opEquals = Binary("==", 5, Left)
let opAnd = Binary("&&", 6, Left)
let opOr = Binary("||", 7, Left)
let opCoalesce = Binary("??", 8, Left)
let opConcat = Binary("++", 9, Left)
let opTernaryConditional = Ternary("?", ":", 10, Right)
let opComma = Binary(",", 11, Left)

let allOperators =
    [ opMembership
      opMembershipForgiving
      opMembershipBracketed
      opMembershipForgivingBracketed
      opNot
      Unary("+", 1, Right)
      Unary("-", 1, Right)
      Binary("*", 2, Left)
      Binary("/", 2, Left)
      Binary("%", 2, Left)
      Binary("+", 3, Left)
      Binary("-", 3, Left)
      Binary("<", 4, Left)
      Binary("<=", 4, Left)
      Binary(">", 4, Left)
      Binary(">=", 4, Left)
      opNotEquals
      opEquals
      opAnd
      opOr
      opCoalesce
      opConcat
      opTernaryConditional
      opComma ]

let unaryOperatorLookup =
    allOperators
    |> Seq.collect (function
        | Unary(name, _, _) as op -> [ name, op ]
        | _ -> [])
    |> Map.ofSeq

let binaryOperatorLookup =
    allOperators
    |> Seq.collect (function
        | Binary(name, _, _) as op -> [ name, op ]
        | BinaryBracketed(name, _, _, _) as op -> [ name, op ]
        | _ -> [])
    |> Map.ofSeq

let precedenceBracketed = 100

let functionToOperator, operatorToFunction =
    let list =
        [ "not", unaryOperatorLookup.["!"]
          "mul", binaryOperatorLookup.["*"]
          "div", binaryOperatorLookup.["/"]
          "mod", binaryOperatorLookup.["%"]
          "add", binaryOperatorLookup.["+"]
          "sub", binaryOperatorLookup.["-"]
          "less", binaryOperatorLookup.["<"]
          "lessOrEquals", binaryOperatorLookup.["<="]
          "greater", binaryOperatorLookup.[">"]
          "greaterOrEquals", binaryOperatorLookup.[">="]
          "equals", binaryOperatorLookup.["=="]
          "and", opAnd
          "or", opOr
          "if", opTernaryConditional
          "coalesce", opCoalesce
          "concat", opConcat ]

    Map.ofList list, Map.ofList (list |> List.map (fun (a, b) -> b, a))

let internallyChainableOperators =
    // These operators correspond to functions which accept multiple arguments
    Set.ofArray [| opConcat; opCoalesce; opAnd; opOr |]
