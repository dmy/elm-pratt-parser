module Tests exposing (Expr(..), suite)

import Expect exposing (Expectation)
import Parser exposing (..)
import Pratt exposing (..)
import Test exposing (..)



-- Main tests parser


type Expr
    = X
    | Y
    | Z
    | Neg Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Exp Expr Expr
    | Fac Expr


nuds : List (Config Expr -> Parser Expr)
nuds =
    [ constant (token "x") X
    , constant (token "y") Y
    , constant (token "z") Z
    , prefix 3 (symbol "-") Neg
    , prefix 3 (symbol "+") identity
    , \conf ->
        succeed identity
            |. symbol "("
            |= expression conf
            |. symbol ")"
    ]


leds : List (Config Expr -> ( Int, Expr -> Parser Expr ))
leds =
    [ infixLeft 1 (symbol "+") Add
    , infixLeft 1 (symbol "-") Sub
    , infixLeft 2 (symbol "*") Mul
    , infixLeft 2 (symbol "/") Div
    , infixRight 4 (symbol "^") Exp
    , postfix 5 (symbol "!") Fac
    ]


config : Config Expr
config =
    configure
        { nuds = nuds
        , leds = leds
        , spaces = spaces
        }


parser : Parser Expr
parser =
    succeed identity
        |= expression config
        |. end


parse : String -> Result (List DeadEnd) Expr
parse expr =
    run parser expr



-- Mini-calculator for some specific tests


calcConfig : Config Int
calcConfig =
    configure
        { nuds = [ always int ]
        , leds =
            [ infixLeft 1 (symbol "+") (+)
            , infixRight 4 (symbol "^") (^)
            ]
        , spaces = succeed ()
        }


calc : String -> Result (List DeadEnd) Int
calc =
    run <|
        succeed identity
            |= expression calcConfig
            |. end



-- TESTS


suite : Test
suite =
    describe "Pratt"
        [ describe "Constants"
            [ test "single constant" <|
                \() ->
                    Expect.equal (parse "x") (Ok X)
            ]
        , describe "Prefix operators"
            [ test "simple prefix operator" <|
                \() ->
                    Expect.equal (parse "-x") (Ok <| Neg X)
            , test "nested prefix operators" <|
                \() ->
                    Expect.equal (parse "-++-++-x") (Ok <| Neg (Neg (Neg X)))
            ]
        , describe "Infix operators"
            [ test "left associativity" <|
                \() ->
                    Expect.equal (parse "x+y-z") (Ok <| Sub (Add X Y) Z)
            , test "right associativity" <|
                \() ->
                    Expect.equal (parse "x^y^z") (Ok <| Exp X (Exp Y Z))
            , test "lower precedence first" <|
                \() ->
                    Expect.equal (parse "x+y*z") (Ok <| Add X (Mul Y Z))
            , test "higher precedence first" <|
                \() ->
                    Expect.equal (parse "x*y+z") (Ok <| Add (Mul X Y) Z)
            ]
        , describe "Postfix operators"
            [ test "simple postfix operator" <|
                \() ->
                    Expect.equal (parse "x!") (Ok <| Fac X)
            , test "nested postfix operators" <|
                \() ->
                    Expect.equal (parse "x!!!") (Ok <| Fac (Fac (Fac X)))
            ]
        , describe "Expressions"
            [ test "parentheses" <|
                \() ->
                    Expect.equal (parse "x*(y+z)") (Ok <| Mul X (Add Y Z))
            , test "nested parentheses" <|
                \() ->
                    Expect.equal (parse "(((x)))") (Ok X)
            , test "top-level expression" <|
                \() ->
                    Expect.equal
                        (run (expression config) "x+y")
                        (run (subExpression 0 config) "x+y")
            ]
        , describe "Mixed operators"
            [ test "identical infix and prefix operators" <|
                \() ->
                    Expect.equal (parse "x--y") (Ok <| Sub X (Neg Y))
            , test "prefix, infix and suffix operators" <|
                \() ->
                    Expect.equal (parse "-x+y!") (Ok <| Add (Neg X) (Fac Y))
            , test "nested infix and postfix operators" <|
                \() ->
                    Expect.equal (parse "-+x!!") (Ok <| Neg (Fac (Fac X)))
            , test "prefix operator before right associative infix operators" <|
                \() ->
                    Expect.equal (parse "-x^y^z") (Ok <| Neg (Exp X (Exp Y Z)))
            , test "prefix operator inside right associative infix operators" <|
                \() ->
                    Expect.equal (parse "x^-y^z") (Ok <| Exp X (Neg (Exp Y Z)))
            , test "prefix operator after right associative infix operators" <|
                \() ->
                    Expect.equal (parse "x^y^-z") (Ok <| Exp X (Exp Y (Neg Z)))
            , test "all operators" <|
                \() ->
                    Expect.equal
                        (parse "-x+y-z*x/y^z!")
                        (Ok <| Sub (Add (Neg X) Y) (Div (Mul Z X) (Exp Y (Fac Z))))
            ]
        , describe "Whitespaces"
            [ test "leading whitespace" <|
                \() ->
                    Expect.equal (parse " x") (parse "x")
            , test "trailing whitespace" <|
                \() ->
                    Expect.equal (parse "x ") (parse "x")
            , test "prefix operator" <|
                \() ->
                    Expect.equal (parse "- x") (parse "-x")
            , test "prefix operators" <|
                \() ->
                    Expect.equal (parse "- - x") (parse "--x")
            , test "infix operator" <|
                \() ->
                    Expect.equal (parse "x + y") (parse "x+y")
            , test "infix operators" <|
                \() ->
                    Expect.equal (parse "x + y / z") (parse "x+y/z")
            , test "postfix operator" <|
                \() ->
                    Expect.equal (parse "x !") (parse "x!")
            , test "postfix operators" <|
                \() ->
                    Expect.equal (parse "x ! !") (parse "x!!")
            , test "mixed operators and double spaces" <|
                \() ->
                    Expect.equal
                        (parse " -  +  (  (  x  /  y  )  )  !  ! ")
                        (parse "-+((x/y))!!")
            , test "forbidden leading space" <|
                \() ->
                    Expect.equal
                        (calc " 0")
                        (Err [ { col = 1, problem = ExpectingInt, row = 1 } ])
            , test "forbidden trailing space" <|
                \() ->
                    Expect.equal
                        (calc "0 ")
                        (Err [ { col = 2, problem = ExpectingEnd, row = 1 } ])
            ]
        , describe "Optimizations"
            [ test "Left-associative expressions tail-call elimination" <|
                \() ->
                    Expect.equal
                        (calc ("1" ++ String.repeat 20000 "+1"))
                        (Ok 20001)
            , test "Right-associative expressions minimum levels" <|
                -- Only works for node.js default stack size or bigger.
                -- Still useful to detect potential regressions.
                \() ->
                    Expect.equal
                        (calc ("1" ++ String.repeat 480 "^1"))
                        (Ok 1)
            ]
        ]
