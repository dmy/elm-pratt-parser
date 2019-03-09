module Pratt.Advanced exposing
    ( expression
    , Config, configure
    , subExpression
    , constant, prefix
    , infixLeft, infixRight, postfix
    )

{-| `Pratt.Advanced` provides the same API as [`Pratt`](Pratt),
but for [`Parser.Advanced`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser-Advanced). This allows to have custom `context` and `problem` types to improve error messages.


# Parser

@docs expression


# Configuration

@docs Config, configure


# Configuration Helpers

@docs subExpression


## NUD Helpers

@docs constant, prefix


## LED Helpers

@docs infixLeft, infixRight, postfix

-}

import Parser.Advanced
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , lazy
        , loop
        , map
        , oneOf
        , succeed
        )



-- PARSER CONFIGURATION


{-| An opaque type describing the parser configuration.
A `Config` is created using [`configure`](#configure).
-}
type Config c x e
    = Config
        { nuds : List (Config c x e -> Parser c x e)
        , leds : List (Config c x e -> ( Int, e -> Parser c x e ))
        , spaces : Parser c x ()
        }


{-| Just like [`Pratt.configure`](Pratt#configure).
-}
configure :
    { nuds : List (Config c x e -> Parser c x e)
    , leds : List (Config c x e -> ( Int, e -> Parser c x e ))
    , spaces : Parser c x ()
    }
    -> Config c x e
configure conf =
    Config conf



-- PRATT PARSER


{-| Just like [`Pratt.parser`](Pratt#parser).
-}
expression : Config c x e -> Parser c x e
expression =
    subExpression 0


{-| Just like [`Pratt.expression`](Pratt#expression).
-}
subExpression : Int -> Config c x e -> Parser c x e
subExpression bindingPower ((Config conf) as config) =
    succeed identity
        |. conf.spaces
        |= lazy (\_ -> nud config)
        |> andThen (\left -> loop ( config, bindingPower, left ) expressionHelp)


nud : Config c x e -> Parser c x e
nud ((Config conf) as config) =
    oneOf <|
        List.map ((|>) config) conf.nuds


{-| This is the core of the Pratt parser algorithm.
It continues parsing the expression as long as a `led` parser with a
binding power above the current one succeeds.

[`loop`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser#loop)
is used instead of a recursive parser to get Tail-Call Elimination.

Also note that `nud` and `led` parsers may call recursively `expression` or
`subExpression`.

-}
expressionHelp : ( Config c x e, Int, e ) -> Parser c x (Step ( Config c x e, Int, e ) e)
expressionHelp ( (Config conf) as config, rbp, left ) =
    succeed identity
        |. conf.spaces
        |= oneOf
            [ map (\e -> Loop ( config, rbp, e )) (led config rbp left)
            , succeed (Done left)
            ]


led : Config c x e -> Int -> e -> Parser c x e
led ((Config conf) as config) rbp left =
    oneOf <|
        List.filterMap (\toLed -> ledFilter (toLed config) rbp left) conf.leds


ledFilter : ( Int, e -> Parser c x e ) -> Int -> e -> Maybe (Parser c x e)
ledFilter ( lbp, ledParser ) rbp left =
    if lbp > rbp then
        Just (ledParser left)

    else
        Nothing



-- NUD HELPERS


{-| Just like [`Pratt.constant`](Pratt#constant).
-}
constant : Parser c x () -> e -> Config c x e -> Parser c x e
constant constantParser e config =
    map (always e) constantParser


{-| Just like [`Pratt.prefix`](Pratt#prefix).
-}
prefix : Int -> Parser c x () -> (e -> e) -> Config c x e -> Parser c x e
prefix bindingPower operator apply config =
    succeed apply
        |. operator
        |= subExpression bindingPower config



-- LED HELPERS


{-| Just like [`Pratt.infixLeft`](Pratt#infixLeft).
-}
infixLeft : Int -> Parser c x () -> (e -> e -> e) -> Config c x e -> ( Int, e -> Parser c x e )
infixLeft bindingPower =
    infixHelp ( bindingPower, bindingPower )


{-| Just like [`Pratt.infixRight`](Pratt#infixRight).
-}
infixRight : Int -> Parser c x () -> (e -> e -> e) -> Config c x e -> ( Int, e -> Parser c x e )
infixRight bindingPower =
    -- To get right associativity, we use (bindingPower - 1) for the
    -- right expression binding power.
    infixHelp ( bindingPower, bindingPower - 1 )


infixHelp : ( Int, Int ) -> Parser c x () -> (e -> e -> e) -> Config c x e -> ( Int, e -> Parser c x e )
infixHelp ( lbp, rbp ) operator apply config =
    ( lbp
    , \left ->
        succeed (apply left)
            |. operator
            |= subExpression rbp config
    )


{-| Just like [`Pratt.postfix`](Pratt#postfix).
-}
postfix : Int -> Parser c x () -> (e -> e) -> Config c x e -> ( Int, e -> Parser c x e )
postfix bindingPower operator apply config =
    ( bindingPower
    , \left -> map (\_ -> apply left) operator
    )
