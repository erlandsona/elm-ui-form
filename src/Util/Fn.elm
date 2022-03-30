module Util.Fn exposing
    ( curry
    , flip
    , swap
    , uncurry
    )

{-| This module holds Combinators

Combinators: google: To Mock a Mockingbird by Raymond Smullyan
Learn about Eta abstraction & reduction.

TLDR; Combinators are functions with no concrete types
So it's only a's, b's, & c's.
No List, Maybe, Result, etc...

-}


flip : (b -> a -> c) -> a -> b -> c
flip fn a b =
    fn b a


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


curry : (( a, b ) -> c) -> a -> b -> c
curry fn a b =
    fn ( a, b )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry fn ( a, b ) =
    fn a b
