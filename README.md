# elm-pratt-parser [![Elm package](https://img.shields.io/elm-package/v/dmy/elm-pratt-parser.svg)](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/) [![Build Status](https://travis-ci.com/dmy/elm-pratt-parser.svg?branch=master)](https://travis-ci.com/dmy/elm-pratt-parser) 

**Top-Down Operator Precedence Parsing**

Face complex precedence and associativity rules without fear using
[`elm/parser`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser).

```sh
    elm install elm/parser
    elm install dmy/elm-pratt-parser
```

# Overview


Writing parsers using
[`elm/parser`](https://package.elm-lang.org/packages/elm/parser/1.1.0/)
is usually simple and fun, but handling complex operators precedence and
associativity rules in an expression parser
[can be tricky](https://github.com/elm/parser/blob/1.1.0/examples/Math.elm#L144),
or even hard and frustrating for more complex cases.

This library goal is to fix this by adding a single 
[`expression`](Pratt#expression) parser to `elm/parser`:

```
    expression : Config expr -> Parser expr
```

This functions is configured with smaller standard parsers, precedence values
and associativity rules, thanks to a minimalist flexible API, and handles the
whole expression parsing complexity using a simple but powerful algorithm
inherited from the one described by **Vaughan Pratt** in his 1973 paper "Top
Down Operator Precedence" [[1]](#references).
 
Helpers are provided for [literals](Pratt#literal), [constants](Pratt#constant),
[prefix](Pratt#prefix), [infix](Pratt#infixLeft) and [postfix](Pratt#postfix)
operations but custom ones can be defined when needed.

The library is small, has a test suite, benefits from tail-call elimination
for left-associative operations, and allows to produce excellent error messages,
as usual with `elm/parser`, using [`Parser.Advanced`](Pratt.Advanced) if wanted.


# Getting Started

## Calculator Example

Here is a quite complete calculator.

It evaluates the result during parsing, without generating an explicit
intermediate abstract syntax tree (AST), so it directly uses `Float` as the
`expr` type.

```elm
import Parser exposing (..)
import Pratt exposing (..)

-- Parsers used at the start of expressions
-- or after an operator
nuds : List (Config Float -> Parser Float)
nuds =
    [ literal float
    , constant (keyword "pi") pi
    , prefix 3 (symbol "-") negate
    , prefix 5 (keyword "cos") cos
    , \config ->
        -- Sub-expression between parentheses
        succeed identity
            |. symbol "("
            |= subExpression 0 config
            |. symbol ")"
    ]

-- Infix and postfix operations parsers
leds : List (Config Float -> (Int, Float -> Parser Float))
leds =
    [ infixLeft 1 (symbol "+") (+)
    , infixLeft 1 (symbol "-") (-)
    , infixLeft 2 (symbol "*") (*)
    , infixLeft 2 (symbol "/") (/)
      -- Exponentiation is right associative
    , infixRight 4 (symbol "^") (^)
      -- Postfix operator to convert degrees to radians
    , postfix 6 (symbol "°") degrees
    ]

-- Expression parser configuration
conf : Config Float
conf =
    configure
        { nuds = nuds
        , leds = leds
        , spaces = spaces -- eat spaces between parsers
        }

-- Include the expression parser in an upper-level one
parser : Parser Float
parser =
    succeed identity
        |= expression conf
        |. end -- string must end after an expression


-- TESTS
run parser "-2^2^3" --> Ok -(2^(2^3))
run parser "-2^2^3" --> Ok -256

run parser "3--4" --> Ok (3-(-4))
run parser "3--4" --> Ok 7

run parser "cos (2*pi)" --> Ok 1
run parser "cos (2*180°)" --> Ok 1
run parser "1 - cos 360°" --> Ok 0
```

## Step by Step

Let's describe step by step each part of the example.

### 1. Define **nud** parsers

First we define the so-called **nud** parsers, for_"**Nu**ll **D**enotation"_,
because they don't use a preceding expression.

These parsers are used at the start of expressions, or after an operator,
and include among others *literals*, *constants*, *prefix* operations or
*sub-expressions* parsers:


    nuds : List (Config Float -> Parser Float)
    nuds =
        [ literal float
        , constant (keyword "pi") pi
        , prefix 3 (symbol "-") negate
        , prefix 5 (keyword "cos") cos
        , \config ->
            -- Sub-expression between parentheses
            succeed identity
                |. symbol "("
                |= subExpression 0 config
                |. symbol ")"
        ]


Note that [`literal`](Pratt#literal), [`constant`](Pratt#constant) and
[`prefix`](Pratt#prefix) helpers all take a `Parser ()` argument, like `float`,
`keyword pi` or `symbol "-"`, so you have full control on parsing and produced
error messages.

The [`prefix`](Pratt#prefix) helper first needs an `Int` argument, called the
*binding power*. It represents the precedence level, so the higher it is,
the higher the operator precedence level is.

All parsers last argument is a `Config expr`, passed automatically by the
`expression` parser, that allows to call recursively the whole
[`expression`](Pratt#expression) parser, or a sub-expression with a different
*binding power* using [`subExpression`](Pratt#subExpression).  
This is used above  in the last parser for expressions between parentheses and
inside `prefix`, `infix` and `postfix` helpers.  
This is why the type of each `nud` parser is `Config expr -> Parser expr`.

### 2. Define **led** parsers

Then we define the so-called **led** parsers, for_"**Le**ft **D**enotation"_.
These parsers are used after a `nud` parser and take a `left` expression
argument. They typically include *infix* and *postfix* operations parsers:

    leds : List (Config Float -> (Int, Float -> Parser Float))
    leds =
        [ infixLeft 1 (symbol "+") (+)
        , infixLeft 1 (symbol "-") (-)
        , infixLeft 2 (symbol "*") (*)
        , infixLeft 2 (symbol "/") (/)
          -- Exponentiation is right associative
        , infixRight 4 (symbol "^") (^)
          -- Postfix operator to convert degrees to radians
        , postfix 6 (symbol "°") degrees
        ]

Like `nud` parsers, they receive a `Config expr`, but return instead a tuple  
`(Int, expr -> Parser expr)` because they need to provide their *binding power*
to the algorithm, and a `expr -> Parser expr` parser that will be called with
the preceding expression (the *left expression*).  
See [`subExpression`](Pratt#subExpression) documentation to better understand
the algorithm.

### 3. Complete configuration

We can them complete the configuration by adding a `spaces` parser of type
`Parser ()` that will be used between each `nud` and `led` parser.
[`Parser.spaces`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser#spaces)
is typically used, or `succeed ()` if a general parser is not wanted.

    conf : Config Float
    conf =
        configure
            { nuds = nuds
            , leds = leds
            , spaces = spaces -- eat spaces between parsers
            }

### 4. Include the expression parser in an upper level one

Let's require the end of string after our expressions by including our parser
into a top-level `elm/parser` one:

    parser : Parser Float
    parser =
        succeed identity
            |= expression conf
            |. end -- string must end after an expression


**That's it!  
And we have used all the functions provided by the `Pratt` library.**

    
A more complete example is included in the package source code, see
`examples/Calc.elm`.

For a similar example but with an AST using a custom
type, see `examples/Math.elm` instead.

Of course, you can define parsers for any expression, not only mathematical
ones. For example have a look at `examples/Boole.elm` for a start with Boole's
Algebra and an example of `if then else` expressions.

# About Pratt Parsers

The parsers configured by this library use a variant of the algorithm described
by **Vaughan Pratt** in his 1973 paper "Top Down Operator Precedence"
[[1]](#references). Such parsers are usuall called *"Pratt parsers"*,
*"Top-Down Operator Precedence parsers"*, or *"TDOP"* parsers.

This algorithm is used in this library because compared to alternatives, 
my experience confirmed what Vaughan Pratt claimed:
> *"The approach described [...] is very simple to understand, trivial to
> implement, easy to use, extremely efficient in practice if not in theory,
> yet flexible enough to meet most reasonable syntactic needs of users [...].
> Moreover, it deals nicely with error detection."*

Note that the later *"precedence climbing"* algorithm is now often considered
as a special case of the earlier Pratt parsing algorithm.

See [[2]](#references) to read more about them.

At last, Douglas Crockford, who implemented a Javascript parser using a Pratt
parser for JSLint [[4]](#references), said:
> "Another explanation is that his technique is most effective when used in a
> dynamic, functional programming language."

I believe that this has already be proven to be wrong [[5]](#references), and
I hope that this implementation will help confirming it.


# Terminology

The terminology inherited from Vaughan R. Pratt's paper used in this library
is about the *"led"*, *"nud"* and *"binding power"* terms. 

I kept this original terminology because I couldn't find a better one, and
less importantly because it would be kind of deceiving to change it for those
used to it.

***nud***

In this library, `nud` parsers, for "**Nu**ll **D**enotation", are
parsers used at the start of expressions or after operators. They  do not use a
preceding expression and therefore do not take an expression argument.

Common `nud` parsers are literals, constants, or prefix operations parsers.

Here is how they are defined in the original paper:
> *"We will call the code denoted by a token without a preceding expression its
null denotation or nud"*

***led***

In this library, `led` parsers, for "**Le**ft **D**enotation", are
parsers used after a `nud` one and use a preceding expression (*left
expression*). Therefore they require an expression argument.

Common `led` parsers are infix and postfix operations parsers.

Here is how they are defined in the original paper:
> *"We will call the code denoted by a token with a preceding expression its
left denotation or led"*

***Binding Power***

The binding power is a kind of precedence value, so the higher it is, the
higher the precedence of the operator is. This is enough to configure complex
expression parsers, however a better understanding might be required to write
custom parsers configuration helpers. For example, the *binding power* is also
used to achieve right-associative operations.

See [[3]](#references) and the algorithm described in
[`subExpression`](Pratt#subExpression) documentation for more details.

# Design and Implementation Considerations

The main differences with Pratt's design and implementation, that reflects on
this library API, are:

1. *Tokens* are reduced to their minimal form:
    - just a parser for `nud` tokens
    - a binding power and a parser (with an expression argument) for `led`
      tokens

   There is therefore no actual *token* anymore and it is not necessary to
   tokenize the expression before parsing it.

2. `nuds` and `leds` are not stored in a `Dict` using the operator as the key,
instead they are just two lists of parsers used with
[`Parser.oneOf`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser#oneOf).

# References

1. Vaughan R. Pratt, ["Top Down Operator Precedence"](https://tdop.github.io/), 1973.
2. Andy Chu, [Pratt Parsing Index and Updates](https://www.oilshell.org/blog/2017/03/31.html),
2017 (updated regularly)
3. Eli Bendersky, [Top-Down operator precedence parsing](https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing), 2010
4. Douglas Crockford, [Top Down Operator Precedence Douglas Crockford](http://crockford.com/javascript/tdop/tdop.html), 2007
5. Andy Chu, [Pratt Parsers Can Be Statically Typed](https://www.oilshell.org/blog/2016/11/05.html), 2016

# License

BSD-3-Clause

