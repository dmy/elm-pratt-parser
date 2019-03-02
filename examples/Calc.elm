module Calc exposing (run)

import Math exposing (factorial, modulo)
import Parser exposing (..)
import Pratt exposing (..)


nuds : List (Config Float -> Parser Float)
nuds =
    [ constant (keyword "e") e
    , constant (keyword "pi") pi
    , always float
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
    , \config ->
        succeed identity
            |. symbol "("
            |= expression config
            |. symbol ")"
    ]


leds : List (Config Float -> ( Int, Float -> Parser Float ))
leds =
    [ infixLeft 1 (symbol "+") (+)
    , infixLeft 1 (symbol "-") (-)
    , infixLeft 2 (symbol "*") (*)
    , infixLeft 2 (symbol "%") modulo
    , infixLeft 2 (symbol "/") (/)
    , infixRight 4 (symbol "^") (^)
    , postfix 6 (symbol "!") factorial
    , postfix 6 (symbol "°") degrees
    ]


conf : Config Float
conf =
    configure
        { nuds = nuds
        , leds = leds
        , spaces = spaces
        }


parser : Parser Float
parser =
    succeed identity
        |= expression conf
        |. end


run : String -> Result (List DeadEnd) Float
run expr =
    Parser.run parser expr
