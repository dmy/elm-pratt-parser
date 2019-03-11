module Math exposing (Expr, eval, factorial, modulo, parser, run)

import Parser exposing ((|.), (|=), Parser, end, float, keyword, map, succeed, symbol)
import Pratt exposing (constant, infixLeft, infixRight, literal, postfix, prefix)



-- PARSER


type Expr
    = Float Float
    | Neg Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
    | Exp Expr Expr
    | Cos Expr
    | Sin Expr
    | Tan Expr
    | Acos Expr
    | Asin Expr
    | Atan Expr
    | Log Expr
    | Ln Expr
    | Fac Expr
    | Deg Expr


mathExpression : Parser Expr
mathExpression =
    Pratt.expression
        { oneOf =
            [ constant (keyword "e") (Float e)
            , constant (keyword "pi") (Float pi)
            , literal (map Float float)
            , prefix 3 (symbol "-") Neg
            , parenthesizedExpression
            , prefix 3 (symbol "+") identity
            , prefix 5 (keyword "cos") Cos
            , prefix 5 (keyword "sin") Sin
            , prefix 5 (keyword "tan") Tan
            , prefix 5 (keyword "acos") Acos
            , prefix 5 (keyword "asin") Asin
            , prefix 5 (keyword "atan") Atan
            , prefix 5 (keyword "log") Log
            , prefix 5 (keyword "ln") Ln
            ]
        , andThenOneOf =
            [ infixLeft 1 (symbol "+") Add
            , infixLeft 1 (symbol "-") Sub
            , infixLeft 2 (symbol "*") Mul
            , infixLeft 2 (symbol "%") Mod
            , infixLeft 2 (symbol "/") Div
            , infixRight 4 (symbol "^") Exp
            , postfix 6 (symbol "!") Fac
            , postfix 6 (symbol "°") Deg
            ]
        , spaces = Parser.spaces
        }


parenthesizedExpression : Pratt.Config Expr -> Parser Expr
parenthesizedExpression config =
    succeed identity
        |. symbol "("
        |= Pratt.subExpression 0 config
        |. symbol ")"


parser : Parser Expr
parser =
    succeed identity
        |= mathExpression
        |. end



-- EVALUATOR


eval : Expr -> Float
eval expr =
    case expr of
        Float n ->
            n

        Neg n ->
            negate (eval n)

        Add a b ->
            eval a + eval b

        Sub a b ->
            eval a - eval b

        Mul a b ->
            eval a * eval b

        Div a b ->
            eval a / eval b

        Mod a b ->
            modulo (eval a) (eval b)

        Exp a b ->
            eval a ^ eval b

        Cos n ->
            cos (eval n)

        Sin n ->
            sin (eval n)

        Tan n ->
            tan (eval n)

        Acos n ->
            acos (eval n)

        Asin n ->
            asin (eval n)

        Atan n ->
            atan (eval n)

        Log n ->
            logBase 10 (eval n)

        Ln n ->
            logBase e (eval n)

        Fac n ->
            factorial (eval n)

        Deg n ->
            degrees (eval n)


modulo : Float -> Float -> Float
modulo a b =
    a - toFloat (floor (a / b)) * b


factorial : Float -> Float
factorial n =
    if n < 0 then
        -- NaN
        0 / 0

    else
        factorialHelp n 1


factorialHelp : Float -> Float -> Float
factorialHelp n productSoFar =
    if n <= 1 then
        productSoFar

    else
        factorialHelp (n - 1) (n * productSoFar)



-- CALCULATOR


run : String -> Result (List Parser.DeadEnd) Float
run expr =
    case Parser.run parser expr of
        Ok ast ->
            Ok (eval ast)

        Err errors ->
            Err errors
