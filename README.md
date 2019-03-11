# elm-pratt-parser [![Elm package](https://img.shields.io/elm-package/v/dmy/elm-pratt-parser.svg)](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/) [![Build Status](https://travis-ci.com/dmy/elm-pratt-parser.svg?branch=master)](https://travis-ci.com/dmy/elm-pratt-parser) 

**Top-Down Operator Precedence Parsing**

Face complex precedence and associativity rules without fear using
[`elm/parser`](https://package.elm-lang.org/packages/elm/parser/1.1.0/).

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
[`expression`](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#expression)
parser to `elm/parser`:

```
expression :
    { oneOf : List (Config expr -> Parser expr)
    , andThenOneOf : List (Config expr -> ( Int, expr -> Parser expr ))
    , spaces : Parser ()
    }
    -> Parser expr
```

This functions is configured with smaller standard parsers, precedence values
and associativity rules, thanks to a minimalist flexible API, and handles the
whole expression parsing complexity using a simple but powerful algorithm
inherited from the one described by **Vaughan Pratt** in his 1973 paper **"Top
Down Operator Precedence"** [[1]](#references).
 
Helpers are provided for
[literals](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#literal),
[constants](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#constant),
[prefix](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#prefix),
[infix](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#infixLeft)
and [postfix](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#postfix)
expressions but custom ones can be defined when needed.

The library is small, has a test suite, benefits from tail-call elimination
for left-associative operations, and allows to produce excellent error messages,
as usual with `elm/parser`, using
[`Parser.Advanced`](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt.Advanced)
if wanted.


# Getting Started

## Calculator Example

Here is a quite complete calculator.

It evaluates the result during parsing, without generating an explicit
intermediate abstract syntax tree (AST), so it directly uses `Float` as the
`expr` type.

```elm
import Parser exposing ((|.), (|=), Parser, float, keyword, symbol)
import Pratt exposing (constant, infixLeft, infixRight, literal, postfix, prefix)


mathExpression : Parser Float
mathExpression =
    Pratt.expression
        { oneOf =
            [ literal float
            , constant (keyword "pi") pi
            , prefix 3 (symbol "-") negate
            , prefix 5 (keyword "cos") cos
            , parenthesizedExpression
            ]
        , andThenOneOf =
            [ infixLeft 1 (symbol "+") (+)
            , infixLeft 1 (symbol "-") (-)
            , infixLeft 2 (symbol "*") (*)
            , infixLeft 2 (symbol "/") (/)
            , infixRight 4 (symbol "^") (^)
            , postfix 6 (symbol "°") degrees
            ]
        , spaces = Parser.spaces
        }


parenthesizedExpression : Pratt.Config Float -> Parser Float
parenthesizedExpression config =
    Parser.succeed identity
        |. symbol "("
        |= Pratt.subExpression 0 config
        |. symbol ")"


math : Parser Float
math =
    Parser.succeed identity
        |= mathExpression
        -- string must end after an expression:
        |. Parser.end


Parser.run math "-2^2^3" --> Ok -(2^(2^3))
Parser.run math "-2^2^3" --> Ok -256

Parser.run math "3--4" --> Ok (3-(-4))
Parser.run math "3--4" --> Ok 7

Parser.run math "cos (2*pi)" --> Ok 1
Parser.run math "cos (2*180°)" --> Ok 1
Parser.run math "1 - cos 360°" --> Ok 0
```

## Step by Step

Let's describe step by step each part of the example.

**1.**
First we configure the parsers used at the start of an expression or after an
operator. The expression parser cannot work without at least one of these
parsers succeeding as it would not be able to parse an `expr` value.
These parsers include among others: *literals*, *constants*, *prefix* 
expressions or sub-expressions parsers.


    mathExpression : Parser Float
    mathExpression =
        Pratt.expression
            { oneOf =
                [ literal float
                , constant (keyword "pi") pi
                , prefix 3 (symbol "-") negate
                , prefix 5 (keyword "cos") cos
                , parenthesizedExpression
                ]


Note that
[`literal`](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#literal),
[`constant`](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#constant)
and [`prefix`](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#prefix)
helpers all use a `Parser` argument, like `float`,
`keyword pi` or `symbol "-"` here, so you have full control on parsing and
produced error messages.

The [`prefix`](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#prefix)
helper first needs an `Int` argument, the
***precedence***. The higher it is, the higher the operator *precedence* is.

All parsers last parameter is a `Config expr`, passed automatically by the
`expression` parser, that allows to call recursively
[`subExpression`](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#subExpression)
with a custom *precedence*.  
This is used here in the parser for sub-expressions between parentheses and
also inside `prefix` and `infix` helpers.  
This is why the type of each `oneOf` parser is
[`Config expr -> Parser expr`](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#expression).


    parenthesizedExpression : Pratt.Config Float -> Parser Float
    parenthesizedExpression config =
        Parser.succeed identity
            |. symbol "("
            |= Pratt.subExpression 0 config
            |. symbol ")"


Note that `expression` is equivalent to `subExpression 0`, so the
[`expression`](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#expression)
parser starts parsing the expression with the lowest *precedence*.

**2.**
Then we configure the parsers that use the result of the previous parser.
As they use the previously parsed expression, they have an `expr` parameter.
They typically include *infix* and *postfix* expressions parsers:


            , andThenOneOf =
                [ infixLeft 1 (symbol "+") (+)
                , infixLeft 1 (symbol "-") (-)
                , infixLeft 2 (symbol "*") (*)
                , infixLeft 2 (symbol "/") (/)
                , infixRight 4 (symbol "^") (^)
                , postfix 6 (symbol "°") degrees
                ]

 
Like configuration `oneOf` parsers, they receive a `Config expr`, but return
instead a tuple  `(Int, expr -> Parser expr)` because they need to provide their
*precedence* to the algorithm, and a `expr -> Parser expr` parser that will
be called with the preceding expression (the *left expression*).  

See [`subExpression`](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#subExpression)
documentation to better understand the algorithm.

**3.** We can then complete the configuration by adding a `spaces : Parser ()`
parser that will be used between each previously configured parser.  
[`Parser.spaces`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser#spaces)
is used here, but `succeed ()` could have been used if a general parser was not
wanted.


            , spaces = Parser.spaces
            }


**4.** At last we require the end of string after our expression by including our
parser into a top-level `elm/parser` one:


    math : Parser Float
    math =
        Parser.succeed identity
            |= mathExpression
            -- string must end after an expression:
            |. Parser.end


**That's it!  
And we have used all the functions exposed by the `Pratt` module.**

    
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

This algorithm is used in this library because my experience comparing
alternatives confirmed for the most part what Vaughan Pratt claimed:
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

I believe that the `dynamic` part has already been proven wrong
[[5]](#references), and I hope that this implementation will help confirming it.


# Terminology

The terminology used in Vaughan R. Pratt's paper is not really intuitive
nor mainstream so it has been changed in this library:
* Configuration `oneOf` parsers are called in the paper `nud`,
for "**Nu**ll **D**enotation". Here is how they are defined in the original
paper:
> *"We will call the code denoted by a token without a preceding expression its
null denotation or nud"*
* Configuration `andThenOneOf` parsers are called `led`, for "**Le**ft
**D**enotation". Here is how they are defined in the original paper:
> *"We will call the code denoted by a token with a preceding expression its
left denotation or led"*
* The *precedence* is called *binding power* in the paper. It is used for
operators precedence, and also allows to achieve right-associative operations.

See [[3]](#references) and the algorithm described in
[`subExpression`](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt#subExpression)
documentation for more details.

# Design and Implementation Considerations

The main differences with Pratt's design and implementation, that reflects on
this library API, are:

1. *Tokens* are reduced to their minimal form:
    - just a parser for `nud` tokens
    - a *precedence* (alias *binding power*) and a parser
    (with an expression parameter) for `led` tokens

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

