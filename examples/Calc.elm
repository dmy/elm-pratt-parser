module Calc exposing (run)

import Math exposing (factorial, modulo)
import Parser exposing ((|.), (|=), Parser, end, float, keyword, succeed, symbol)
import Pratt exposing (constant, infixLeft, infixRight, literal, postfix, prefix)


mathExpression : Parser Float
mathExpression =
    Pratt.expression
        { oneOf =
            [ constant (keyword "e") e
            , constant (keyword "pi") pi
            , literal float
            , prefix 3 (symbol "-") negate
            , prefix 3 (symbol "+") identity
            , prefix 5 (keyword "cos") cos
            , prefix 5 (keyword "sin") sin
            , prefix 5 (keyword "tan") tan
            , prefix 5 (keyword "acos") acos
            , prefix 5 (keyword "asin") asin
            , prefix 5 (keyword "atan") atan
            , prefix 5 (keyword "log") (logBase 10)
            , prefix 5 (keyword "ln") (logBase e)
            , parenthesizedExpression
            ]
        , andThenOneOf =
            [ infixLeft 1 (symbol "+") (+)
            , infixLeft 1 (symbol "-") (-)
            , infixLeft 2 (symbol "*") (*)
            , infixLeft 2 (symbol "%") modulo
            , infixLeft 2 (symbol "/") (/)
            , infixRight 4 (symbol "^") (^)
            , postfix 6 (symbol "!") factorial
            , postfix 6 (symbol "°") degrees
            ]
        , spaces = Parser.spaces
        }


parenthesizedExpression : Pratt.Config Float -> Parser Float
parenthesizedExpression config =
    succeed identity
        |. symbol "("
        |= Pratt.subExpression 0 config
        |. symbol ")"


parser : Parser Float
parser =
    succeed identity
        |= mathExpression
        |. end


run : String -> Result (List Parser.DeadEnd) Float
run expr =
    Parser.run parser expr
