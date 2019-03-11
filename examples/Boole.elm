module Boole exposing (run)

import Parser exposing ((|.), (|=), Parser, end, keyword, succeed, symbol, token)
import Pratt exposing (constant, infixLeft, prefix)


booleanExpression : Parser Bool
booleanExpression =
    Pratt.expression
        { oneOf =
            [ constant (token "1") True
            , constant (token "0") False
            , prefix 3 (keyword "not") not
            , ifThenElse
            , parenthesizedExpression
            ]
        , andThenOneOf =
            [ infixLeft 2 (keyword "and") (&&)
            , infixLeft 1 (keyword "or") (||)
            ]
        , spaces = Parser.spaces
        }


ifThenElse : Pratt.Config Bool -> Parser Bool
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


parenthesizedExpression : Pratt.Config Bool -> Parser Bool
parenthesizedExpression config =
    succeed identity
        |. symbol "("
        |= Pratt.subExpression 0 config
        |. symbol ")"


parser : Parser Bool
parser =
    succeed identity
        |= booleanExpression
        |. end


{-| Example:

    Boole.run "if not (1 and 0) then 0 or 1 else 0"
    --> Ok True

-}
run : String -> Result (List Parser.DeadEnd) Bool
run str =
    Parser.run parser str
