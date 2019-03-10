module Pratt exposing
    ( expression
    , Config, configure
    , subExpression
    , literal, constant, prefix
    , infixLeft, infixRight, postfix
    )

{-|

  - **Parser**: [`expression`](#expression)
  - **Configuration**: [`Config`](#Config) [`configure`](#configure)
  - **Configuration helpers**: [`subExpression`](#subExpression)
      - **nud** helpers: [`literal`](#literal) [`constant`](#constant)
        [`prefix`](#prefix)
      - **led** helpers: [`infixLeft`](#infixLeft)
        [`infixRight`](#infixRight) [`postfix`](#postfix)


# Parser

@docs expression


# Configuration

@docs Config, configure


# Configuration Helpers

@docs subExpression


## **nud** helpers

@docs literal, constant, prefix


## **led** helpers

@docs infixLeft, infixRight, postfix

-}

import Parser exposing (Parser, Problem)
import Pratt.Advanced as Advanced



-- PARSER CONFIGURATION


{-| An opaque type based on
[`Pratt.Advanced.Config`](Pratt.Advanced#Config)
representing the parser configuration.
A `Config` is created using [`configure`](#configure).
-}
type alias Config expr =
    Advanced.Config Never Problem expr


{-| Build a [`Config`](#Config) from `nud`, `led` and `spaces` parsers.

**`nuds`:**

> A list of _"**Nu**ll **D**enotation"_ parsers that do **not** take an
> expression on the left. They will be tried successively by the parser using
> `Parser.oneOf` and the parser `Config`.

> Examples: parsers for literals, constants, prefix expressions or a
> sub-expression between parentheses.

**`leds`:**

> A list of _"**Le**ft **D**enotation"_ parsers that **do** take an expression
> on the left and have an `Int` _binding power_. The ones that have a binding
> power above the current one (`0` by default) will be tried by the parser using
> `Parser.oneOf`, the parser `Config` and the current _left_ expression (the
> expression returned by the previous parser).

> Examples: parsers for prefix and postfix expressions.

**`spaces`:**

> A parser called before and after each `nud` and `led` parser, typically used
> to consume whitespaces.

> If a more specific behavior is needed, this parser can be ignored by using
> `succeed ()` and a custom behavior added inside `nuds` and `leds` parsers.

**Notes:**

  - The `nud` and `led` parsers are parameterized by a [`Config`](#Config)
    to be able to call [`expression`](#expression) and
    [`subExpression`](#subExpression), which are the main building
    blocks for `nud` and `led` helpers. This `Config` will be automatically
    passed by the parser.
  - The parser will not use
    [`Parser.backtrackable`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser#backtrackable),
    so it is up to you to correctly setup your parsers to avoid having failed
    parsers consuming characters.

For example, a basic calculator could be configured like this:

    import Parser exposing (..)
    import Pratt exposing (..)

    nuds : List (Config Float -> Parser Float)
    nuds =
        [ literal float
        , prefix 3 (symbol "-") negate
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
        , infixLeft 2 (symbol "/") (/)
        , infixRight 4 (symbol "^") (^)
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
            |= expression  conf
            |. end


    run parser "-1*3--5+4/2^2" --> Ok ((-1*3)-(-5)+(4/(2^2)))
    run parser "-1*3--5+4/2^2" --> Ok 3
    run parser "((-1*3)-(-5)+(4/(2^2)))" --> Ok 3

-}
configure :
    { nuds : List (Config expr -> Parser expr)
    , leds : List (Config expr -> ( Int, expr -> Parser expr ))
    , spaces : Parser ()
    }
    -> Config expr
configure =
    Advanced.configure



-- PRATT PARSER


{-| Build a parser based on the given [configuration](#configuration) that can
be combined with other
[`elm/parser`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser)
parsers.

The top level expression is parsed with a _binding power_ of `0`,
see [`subExpression`](#subExpression).

For example a parser that, given a [`Config`](#Config), requires the end of the
string after having parsed successfully an expression, can be written like this:

    import Parser exposing (..)
    import Pratt exposing (..)

    parser : Config expr -> Parser expr
    parser config =
        succeed identity
            |= expression config
            |. end

Let's test it with a simple integer `nud` parser:

    nuds : List (Config Int -> Parser Int)
    nuds =
        [ literal int ]

    conf : Config Int
    conf =
        configure
            { nuds = nuds
            , leds = []
            , spaces = succeed ()
            }

    run (parser conf) "1" --> Ok 1

Our `spaces` parser is set to `succeed ()` and therefore does nothing,
so trailing spaces are forbidden:

    run (parser conf) "1 "
    --> Err [{ row = 1, col = 2, problem = ExpectingEnd }]

Without `Parser.end`, the parser will succeed, consuming only the first
expression:

    run (expression conf) "1 2" --> Ok 1

-}
expression : Config expr -> Parser expr
expression =
    Advanced.expression


{-| Build an expression parser based on the given _binding power_ and
configuration.

This is the core function of the parser.
The [`expression`](#expression) function is actually implemented using
`subExpression` with a _binding power_ of `0`, like this:

    expression : Config expr -> Parser expr
    expression config =
        subExpression 0 config

`subExpression` and `expression` are also used to make `led` and `nud` helpers.

For example [`prefix`](#prefix) can be implemented like this:

    prefix : Int -> Parser () -> (e -> e) -> Config expr -> Parser expr
    prefix bindingPower operator apply config =
        succeed apply
            |. operator
            |= subExpression bindingPower config

A parser for sub-expressions between parentheses like this:

    parens : Config Expr -> Parser Expr
    parens config =
        succeed identity
            |. symbol "("
            |= expression config
            |. symbol ")"

Note that it could also be written using `prefix`:

    parens : Config Expr -> Parser Expr
    parens config =
        prefix 0 (symbol "(") identity config
            |. symbol ")"

**Algorithm**:

`subExpression` uses the following algorithm:

1.  Use the `Int` _binding power_ argument as the current _binding power_.

2.  Run the `spaces` parser.

3.  Try `nuds` parsers successively in order using
    [`Parser.oneOf`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser#oneOf)
    until one is chosen (i.e. until one of them consumes characters).
    This parser may call `expression` or `subExpression` recursively with
    different _binding power_ values.
    If no parser succeeds, the whole parser fails, else the expression returned
    by the `nud` parser is used as the _left_ expression in the next steps.

4.  Run the `spaces` parser.

5.  Select `leds` parsers that have a _binding power_ above the current one,
    try them successively using `Parser.oneOf` with the _left_ expression
    argument until one is chosen. This parser may also call `expression` or
    `subExpression`recursively with different _binding power_ values.

6.  If no `led` parser succeeds, return the _left_ expression.
    Else, loop from 4. using the expression just parsed by the `led` parser
    as the new _left_ expression.

-}
subExpression : Int -> Config expr -> Parser expr
subExpression =
    Advanced.subExpression



-- NUD HELPERS


{-| Build a `nud` parser for a literal.

The `Config` argument is passed automatically by the parser.

    import Parser exposing (..)
    import Pratt exposing (..)

    type Expr
        = Int Int
        | Float Float

    digits : Parser Expr
    digits =
        number
            { int = Just Int
            , hex = Just Int
            , octal = Nothing
            , binary = Nothing
            , float = Just Float
            }

    nuds : List (Config Expr -> Parser Expr)
    nuds =
        [ literal digits ]

    conf : Config Expr
    conf =
        configure
            { nuds = nuds, leds = [], spaces = spaces }

    run (expression conf) "1234" --> Ok (Int 1234)
    run (expression conf) "0x1b" --> Ok (Int 27)
    run (expression conf) "3.14159" --> Ok (Float 3.14159)

**Note:** if you want to able to handle expressions like `3--4`, you could
have a negation prefix parser like `prefix 3 (-) Neg` declared before
a `digits` literal and let `digits` only handle positive numbers.

-}
literal : Parser expr -> Config expr -> Parser expr
literal =
    Advanced.literal


{-| Build a `nud` parser for a constant.

The `Config` argument is passed automatically by the parser.

    import Parser exposing (..)
    import Pratt exposing (..)

    nuds : List (Config Float -> Parser Float)
    nuds =
        [ constant (keyword "pi") pi ]

    conf : Config Float
    conf =
        configure
            { nuds = nuds, leds = [], spaces = spaces }

    run (expression conf) "pi" --> Ok pi

-}
constant : Parser () -> expr -> Config expr -> Parser expr
constant =
    Advanced.constant


{-| Build a `nud` parser for a prefix expression with a given _binding power_.

The `Config` argument is passed automatically by the parser.

    import Parser exposing (..)
    import Pratt exposing (..)

    nuds : List (Config Int -> Parser Int)
    nuds =
        [ literal int
        , prefix 3 (symbol "-") negate
        , prefix 3 (symbol "+") identity
        ]

    conf : Config Int
    conf =
        configure
            { nuds = nuds, leds = [], spaces = spaces }

    run (expression conf) "-1" --> Ok -1
    run (expression conf) "--1" --> Ok 1
    run (expression conf) "+1" --> Ok 1
    run (expression conf) "+-+-1" --> Ok 1

`prefix` can also be used to build more complex `nud` helpers, for example:

    type Expr
        = IfThenElse Expr Expr Expr

    ifThenElse : Config Expr -> Parser Expr
    ifThenElse config =
        succeed IfThenElse
            |= prefix 0 (keyword "if") identity config
            |= prefix 0 (keyword "then") identity config
            |= prefix 0 (keyword "else") identity config

-}
prefix : Int -> Parser () -> (expr -> expr) -> Config expr -> Parser expr
prefix =
    Advanced.prefix



-- LED HELPERS


{-| Build a `led` parser for an infix expression with a left-associative operator
and a given _binding power_.

The `Config` argument is passed automatically by the parser.

    import Parser exposing (..)
    import Pratt exposing (..)

    nuds : List (Config Float -> Parser Float)
    nuds =
        [ literal float ]

    leds : List (Config Float -> (Int, Float -> Parser Float))
    leds =
        [ infixLeft 1 (symbol "+") (+)
        , infixLeft 1 (symbol "-") (-)
        , infixLeft 2 (symbol "*") (*)
        , infixLeft 2 (symbol "/") (/)
        ]

    conf : Config Float
    conf =
        configure
            { nuds = nuds, leds = leds, spaces = spaces }

    run (expression conf) "5+4-3*2/1" --> Ok (5+4-(3*2/1))
    run (expression conf) "5+4-3*2/1" --> Ok 3

-}
infixLeft : Int -> Parser () -> (expr -> expr -> expr) -> Config expr -> ( Int, expr -> Parser expr )
infixLeft =
    Advanced.infixLeft


{-| Build a `led` parser for an infix expression with a right-associative operator
and a given _binding power_.

The `Config` argument is passed automatically by the parser.

    import Parser exposing (..)
    import Pratt exposing (..)

    nuds : List (Config Float -> Parser Float)
    nuds =
        [ literal float ]

    leds : List (Config Float -> (Int, Float -> Parser Float))
    leds =
        [ infixRight 4 (symbol "^") (^) ]

    conf : Config Float
    conf =
        configure
            { nuds = nuds, leds = leds, spaces = spaces }

    run (expression conf) "2^2^3" --> Ok (2^(2^3))
    run (expression conf) "2^2^3" --> Ok 256

**Note:** As usual in Pratt parsers, right-associativity is achieved by parsing the right
expression with the _binding power_ of the infix operator minus 1.

-}
infixRight : Int -> Parser () -> (expr -> expr -> expr) -> Config expr -> ( Int, expr -> Parser expr )
infixRight =
    Advanced.infixRight


{-| Build a `led` parser for a postfix expression with a given _binding power_.

The `Config` argument is passed automatically by the parser.

    import Parser exposing (..)
    import Pratt exposing (..)

    nuds : List (Config Float -> Parser Float)
    nuds =
        [ literal float ]


    leds : List (Config Float -> (Int, Float -> Parser Float))
    leds =
        [ postfix 6 (symbol "°") degrees ]

    conf : Config Float
    conf =
        configure
            { nuds = nuds, leds = leds, spaces = spaces }

    run (expression conf) "180°" --> Ok pi
    run (expression conf) "360°" --> Ok (2*pi)

-}
postfix : Int -> Parser () -> (expr -> expr) -> Config expr -> ( Int, expr -> Parser expr )
postfix =
    Advanced.postfix
