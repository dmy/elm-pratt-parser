module Math exposing (Expr, eval, factorial, modulo, parser, run)

import Parser exposing (..)
import Pratt exposing (..)



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


nuds : List (Config Expr -> Parser Expr)
nuds =
    [ constant (keyword "e") (Float e)
    , constant (keyword "pi") (Float pi)
    , literal (map Float float)
    , prefix 3 (symbol "-") Neg
    , parens
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


parens : Config Expr -> Parser Expr
parens config =
    succeed identity
        |. symbol "("
        |= expression config
        |. symbol ")"


leds : List (Config Expr -> ( Int, Expr -> Parser Expr ))
leds =
    [ infixLeft 1 (symbol "+") Add
    , infixLeft 1 (symbol "-") Sub
    , infixLeft 2 (symbol "*") Mul
    , infixLeft 2 (symbol "%") Mod
    , infixLeft 2 (symbol "/") Div
    , infixRight 4 (symbol "^") Exp
    , postfix 6 (symbol "!") Fac
    , postfix 6 (symbol "°") Deg
    ]


conf : Config Expr
conf =
    configure
        { nuds = nuds
        , leds = leds
        , spaces = spaces
        }


parser : Parser Expr
parser =
    succeed identity
        |= expression conf
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


run : String -> Result (List DeadEnd) Float
run expr =
    case Parser.run parser expr of
        Ok ast ->
            Ok (eval ast)

        Err errors ->
            Err errors
