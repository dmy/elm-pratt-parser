# Top-Down Operator Precedence Parser

[![Build Status](https://travis-ci.com/dmy/elm-pratt-parser.svg?branch=master)](https://travis-ci.com/dmy/elm-pratt-parser)

Handling complex operators associativity and precedence rules in an expression
parser is not effortless using
[`elm/parser`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser).

This library allows to build such parsers from a few helpers and aims to avoid
restricting customization, performance or errors messages quality.
The resulting parsers can be combined with other ones from `elm/parser` as usual.

## Pratt Parsers

The parsers built by this library use a variant of the algorithm described by
Vaughan Pratt in his 1973 paper "Top Down Operator Precedence"
[[1]](#references).

Such parsers are called *"Pratt parsers"*, *"Top-Down Operator Precedence
parsers"*, or *"TDOP"* parsers.

The later *"precedence climbing"* algorithm is now usually considered as a
special case of the earlier Pratt parsing algorithm.

See [[2]](#references) to read more about them.

## Terminology

The minimum terminology inherited from Vaughan R. Pratt's paper and required
to use this library is about the *"led"*, *"nud"* and *"binding power"* terms. 

***NUD***

In this library, "NUD" parsers, for "**Nu**ll **D**enotation", are
parsers that do not use a preceding expression.
Therefore they do not take an expression argument.

Common `NUD` parsers are constants, literals or prefix expressions parsers.

***LED***

In this library, "LED" parsers, for "**Le**ft **D**enotation", are
parsers that use a preceding expression, or *left expression*.
Therefore they require an expression argument.

Common `LED` parsers are infix and postfix operations parsers.

***Binding Power***

The binding power is a kind of precedence value, so the higher it is, the
higher the precedence of the operator is. This is enough to configure complex
expression parsers, however a better understanding might be required to write
custom parsers configuration helpers.

See [[3]](#references) for more details.

## Design and Implementation Considerations

The main differences with the original one, reflecting on the API, are:

1. *Tokens* are reduced to their minimal form:
    - just a parser for `NUD` tokens
    - a binding power and a parser (with an expression argument) for `LED`
      tokens

   There is therefore no actual *token* anymore and it is not necessary to
   tokenize the expression before parsing it.

2. NUDs and LEDs are not stored in a `Dict` using the operator as the key,
instead they are just two lists of parsers used with
[`Parser.oneOf`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser#oneOf).

## Calculator Example

This example shows how to build a quite typical calculator.
The result is actually evaluated during parsing, without generating an explicit
intermediate abstract syntax tree (AST).

A slightly more complete example is included in the package source code,
see `examples/Calc.elm`. For a similar example but with an AST using a custom
type, see `examples/Math.elm` instead.

```elm
module Calc exposing (run)

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
    , infixLeft 2 (symbol "/") (/)
    , infixRight 4 (symbol "^") (^)
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



-- TESTS

run "-2^2^3" --> Ok -256
run "3--4" --> Ok 7
run "cos (2*pi)" --> Ok 1
run "ln e - cos 360°" --> Ok 0
```

## References

1. Vaughan R. Pratt, ["Top Down Operator Precedence"](https://tdop.github.io/), 1973.
2. Andy Chu, [Pratt Parsing Index and Updates](https://www.oilshell.org/blog/2017/03/31.html),
2017 (updated regularly)
3. Eli Bendersky, [Top-Down operator precedence parsing](https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing), 2010

## License

BSD-3-Clause
