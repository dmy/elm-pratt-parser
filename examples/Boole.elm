module Boole exposing (run)

import Parser exposing (..)
import Pratt exposing (..)


nuds : List (Config Bool -> Parser Bool)
nuds =
    [ constant (token "1") True
    , constant (token "0") False
    , prefix 3 (keyword "not") not
    , ifThenElse
    , parens
    ]


ifThenElse : Config Bool -> Parser Bool
ifThenElse config =
    succeed ifThenElseHelp
        |= prefix 0 (keyword "if") identity config
        |= prefix 0 (keyword "then") identity config
        |= prefix 0 (keyword "else") identity config


ifThenElseHelp : Bool -> a -> a -> a
ifThenElseHelp cond thenExpr elseExpr =
    if cond then
        thenExpr

    else
        elseExpr


parens : Config Bool -> Parser Bool
parens config =
    prefix 0 (symbol "(") identity config
        |. symbol ")"


leds : List (Config Bool -> ( Int, Bool -> Parser Bool ))
leds =
    [ infixLeft 2 (keyword "and") (&&)
    , infixLeft 1 (keyword "or") (||)
    ]


conf : Config Bool
conf =
    configure
        { nuds = nuds
        , leds = leds
        , spaces = spaces
        }


parser : Parser Bool
parser =
    succeed identity
        |= expression conf
        |. end


{-| Examples:

    Boolean.run "if not (1 and 0) then 0 or 1 else 0"
    --> Ok True

-}
run : String -> Result (List DeadEnd) Bool
run str =
    Parser.run parser str
