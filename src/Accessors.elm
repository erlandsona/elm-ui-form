module Accessors exposing
    ( Relation, Accessor, Lens, Lens_, Setable
    , get, set, over, name
    , try
    , key
    , each, at, eachIdx
    , every, ix
    , one, two
    , makeOneToOne, makeOneToN
    , makeOneToOne_, makeOneToN_
    --, def
    --, Modifiable
    --, Getable
    )

{-| Relations are interfaces to document the relation between two data
structures. For convenience, we'll call the containing structure `super`, and
the contained structure `sub`. What a `Relation` claims is that a `super` is
referencing a `sub` in some way.

Relations are the building blocks of accessors. An accessor is a function that
expects a `Relation` and builds a new relation with it. Accessors are
composable, which means you can build a chain of relations to manipulate nested
structures without handling the packing and the unpacking.


# Relation

@docs Relation, Accessor, Lens, Lens_, Getable, Setable, Modifiable


# Action functions

Action functions are functions that take an accessor and let you perform a
specific action on data using that accessor.

@docs get, set, over, name


# Common accessors

@docs try, def
@docs key
@docs each, at, eachIdx
@docs every, ix
@docs one, two


# Build your own accessors

Accessors are built using these functions:

@docs makeOneToOne, makeOneToN
@docs makeOneToOne_, makeOneToN_

-}

import Array exposing (Array)
import Dict exposing (Dict)


{-| The most general version of this type that everything else specializes
-}
type alias Accessor dataBefore dataAfter attrBefore attrAfter reachable wrap =
    Relation attrBefore wrap attrAfter
    -> Relation dataBefore reachable dataAfter


{-| This is an approximation of Van Laarhoven encoded Lenses which enable the
the callers to use regular function composition to build more complex nested
updates of more complicated types.

But the original "Lens" type looked more like:

    type alias Lens structure attribute =
        { get : structure -> attribute
        , set : structure -> attribute -> structure
        }

unfortunately these can't be composed without
defining custom `composeLens`, `composeIso`, `composePrism`, style functions.

whereas with this approach we're able to make use of Elm's built in `<<` operator
to get/set/over deeply nested data.

-}
type alias
    Lens
        -- Before Action
        structure
        -- After Action
        transformed
        -- Focus Before action
        attribute
        -- Focus After action
        built
    =
    Relation attribute built transformed
    -> Relation structure built transformed


type alias Lens_ structure attribute =
    Lens structure attribute attribute attribute



-- type alias Getable structure transformed attribute built reachable =
--     Relation attribute built attribute
--     -> Relation structure reachable transformed


type alias Setable structure transformed attribute built =
    Relation attribute attribute built -> Relation structure attribute transformed


{-| A `Relation super sub wrap` is a type describing how to interact with a
`sub` data when given a `super` data.

The `wrap` exists because some types can't ensure that `get` will return a
`sub`. For instance, `Maybe sub` may not actually contain a `sub`. Therefore,
`get` returns a `wrap` which, in that example, will be `Maybe sub`

Implementation: A relation is a banal record storing a `get` function and an
`over` function.

-}
type Relation super sub wrap
    = Relation
        { get : super -> wrap
        , over : (sub -> sub) -> (super -> super)
        , name : String
        }


{-| The get function takes:

  - An accessor,
  - A datastructure with type `super`
    and returns the value accessed by that combinator.

```
get (foo << bar) myRecord
```

-}
get :
    (Relation attribute built attribute
     -> Relation structure reachable transformed
    )
    -> structure
    -> transformed
get accessor s =
    let
        (Relation relation) =
            accessor (Relation { get = \super -> super, over = void, name = "" })
    in
    relation.get s


void : a -> b
void super =
    void super


{-| This function gives the name of the function as a string...
-}
name : Accessor a b c d e f -> String
name accessor =
    let
        (Relation relation) =
            accessor (Relation { get = void, over = void, name = "" })
    in
    relation.name


{-| The set function takes:

  - An accessor,
  - A value of the type `sub`,
  - A datastructure with type `super`
    and it returns the data structure, with the accessible field changed to be
    the set value.

```
set (foo << bar) "Hi!" myRecord
```

-}
set :
    (Relation attribute attribute built
     -> Relation structure attribute transformed
    )
    -> attribute
    -> structure
    -> structure
set accessor value s =
    let
        (Relation relation) =
            accessor (Relation { get = void, over = \fn -> fn, name = "" })

        newSuper =
            relation.over (\_ -> value) s
    in
    newSuper



-- type alias Modifiable =
--    Relation attribute x y -> Relation structure a transformed


{-| The over function takes:

  - An accessor,
  - A function `(sub -> sub)`,
  - A datastructure with type `super`
    and it returns the data structure, with the accessible field changed by applying
    the function to the existing value.

```
over (foo << qux) ((+) 1) myRecord
```

-}
over :
    (Relation attribute attribute y -> Relation structure attribute transformed)
    -> (attribute -> attribute)
    -> structure
    -> structure
over accessor change s =
    let
        (Relation relation) =
            accessor (Relation { get = void, over = \fn -> fn, name = "" })
    in
    relation.over change s


{-| This function lets you build an accessor for containers that have
a 1:1 relation with what they contain, such as a record and one of its fields:

    foo : Relation field sub wrap -> Relation { rec | foo : field } sub wrap
    foo =
        makeOneToOne
            .foo
            (\change rec -> { rec | foo = change rec.foo })

-}
makeOneToOne :
    (super -> sub)
    -> ((sub -> sub) -> super -> super)
    -> Relation sub reachable wrap
    -> Relation super reachable wrap
makeOneToOne =
    makeOneToOne_ ""


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    foo : Relation field sub wrap -> Relation { rec | foo : field } sub wrap
    foo =
        makeOneToOne_
            ".foo"
            .foo
            (\change rec -> { rec | foo = change rec.foo })

-}
makeOneToOne_ :
    String
    -> (structure -> attribute)
    -> ((attribute -> attribute) -> structure -> structure)
    -> Lens structure transformed attribute built
makeOneToOne_ n getter mapper (Relation sub) =
    Relation
        { get = \super -> sub.get (getter super)
        , over = \change super -> mapper (sub.over change) super
        , name = n ++ sub.name
        }


{-| This function lets you build an accessor for containers that have
a 1:N relation with what they contain, such as `List` (0-N cardinality) or
`Maybe` (0-1). E.g.:

    each : Relation elem sub wrap -> Relation (List elem) sub (List wrap)
    each =
        makeOneToN
            List.map
            List.map

n.b. implementing those is usually considerably simpler than the type suggests.

-}
makeOneToN :
    ((attribute -> built) -> structure -> transformed)
    -> ((attribute -> attribute) -> structure -> structure)
    -- What is reachable here? And this is obviously not Lens so?
    -> (Relation attribute reachable built -> Relation structure reachable transformed)
makeOneToN =
    makeOneToN_ ""


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    each : Relation elem sub wrap -> Relation (List elem) sub (List wrap)
    each =
        makeOneToN_ "[]"
            List.map
            List.map

-}
makeOneToN_ :
    String
    -> ((attribute -> built) -> structure -> transformed)
    -> ((attribute -> attribute) -> structure -> structure)
    -- What is reachable here?
    -> Relation attribute reachable built
    -> Relation structure reachable transformed
makeOneToN_ n getter mapper (Relation sub) =
    Relation
        { get = \super -> getter sub.get super
        , over = \change super -> mapper (sub.over change) super
        , name = n ++ sub.name
        }


{-| This accessor combinator lets you access values inside List.

    import Accessors exposing (..)
    import Lens as L

    listRecord : {foo : List {bar : Int}}
    listRecord = { foo = [ {bar = 2}
                         , {bar = 3}
                         , {bar = 4}
                         ]
                 }

    get (L.foo << each << L.bar) listRecord
    --> [2, 3, 4]

    over (L.foo << each << L.bar) ((+) 1) listRecord
    --> {foo = [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
each : Relation attribute built transformed -> Relation (List attribute) built (List transformed)
each =
    makeOneToN_ ":[]" List.map List.map


eachIdx : Relation ( Int, attribute ) reachable ( Int, built ) -> Relation (List attribute) reachable (List built)
eachIdx =
    let
        -- asdf : (( Int, attribute ) -> ( Int, built )) -> List attribute -> List built
        asdf fn =
            List.indexedMap
                (\idx -> Tuple.pair idx >> fn >> Tuple.second)
    in
    makeOneToN_ "#[]" asdf asdf


{-| This accessor combinator lets you access values inside Array.

    import Array exposing (Array)
    import Accessors exposing (..)
    import Lens as L

    arrayRecord : {foo : Array {bar : Int}}
    arrayRecord =
        { foo =
            Array.fromList [{ bar = 2 }, { bar = 3 }, {bar = 4}]
        }

    get (L.foo << every << L.bar) arrayRecord
    --> Array.fromList [2, 3, 4]

    over (L.foo << every << L.bar) ((+) 1) arrayRecord
    --> {foo = Array.fromList [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
every : Relation super sub wrap -> Relation (Array super) sub (Array wrap)
every =
    makeOneToN_ "[]" Array.map Array.map


{-| This accessor combinator lets you access values inside Maybe.

    import Accessors exposing (..)
    import Lens as L

    maybeRecord : { foo : Maybe { bar : Int }, qux : Maybe { bar : Int } }
    maybeRecord = { foo = Just { bar = 2 }
                  , qux = Nothing
                  }

    get (L.foo << try << L.bar) maybeRecord
    --> Just 2

    get (L.qux << try << L.bar) maybeRecord
    --> Nothing

    over (L.foo << try << L.bar) ((+) 1) maybeRecord
    --> {foo = Just {bar = 3}, qux = Nothing}

    over (L.qux << try << L.bar) ((+) 1) maybeRecord
    --> {foo = Just {bar = 2}, qux = Nothing}

-}
try : Relation sub path wrap -> Relation (Maybe sub) path (Maybe wrap)
try =
    makeOneToN_ "?" Maybe.map Maybe.map



--{-| This accessor combinator lets you provide a default value for otherwise failable compositions
--  TODO: Doesn't do what is expected... :/
--    import Dict exposing (Dict)
--    import Lens as L
--    dict : Dict String {bar : Int}
--    dict =
--        Dict.fromList [("foo", {bar = 2})]
--    get (key "foo" << def {bar = 0}) dict
--    --> {bar = 2}
--    get (key "baz" << def {bar = 0}) dict
--    --> {bar = 0}
--    get (key "foo" << try << L.bar << def 0) dict
--    --> 2
--    get (key "baz" << try << L.bar << def 0) dict
--    --> 0
---}
--def : sub -> Relation sub reachable sub -> Relation (Maybe sub) reachable sub
--def d =
--    makeOneToN "??"
--        (\f -> over try f >> Maybe.withDefault d)
--        Maybe.map


{-| This accessor combinator lets you access Dict members.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Dict exposing (Dict)
    import Accessors exposing (..)
    import Lens as L

    dict : Dict String {bar : Int}
    dict = Dict.fromList [("foo", {bar = 2})]

    get (key "foo") dict
    --> Just {bar = 2}

    get (key "baz") dict
    --> Nothing

    get (key "foo" << try << L.bar) dict
    --> Just 2

    set (key "foo") Nothing dict
    --> Dict.remove "foo" dict

    set (key "baz" << try << L.bar) 3 dict
    --> dict

-}
key : comparable -> Relation (Maybe v) reachable wrap -> Relation (Dict comparable v) reachable wrap
key k =
    makeOneToOne_ "{}" (Dict.get k) (Dict.update k)



-- keyed : Relation attribute built transformed -> Relation (Dict comparable v) built transformed
-- keyed =
--     makeOneToN_ "{_}" (\fn -> Dict.map (Tuple.pair >> fn)) (\fn -> Dict.map (Tuple.pair >> fn))


{-| This accessor combinator lets you access Dict members.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Accessors exposing (..)
    import Lens as L

    list : List { bar : String }
    list = [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]

    get (at 1) list
    --> Just { bar = "Things" }

    get (at 9000) list
    --> Nothing

    get (at 0 << L.bar) list
    --> Just "Stuff"

    set (at 0 << L.bar) "Whatever" list
    --> [{ bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" }]

    set (at 9000 << L.bar) "Whatever" list
    --> list

-}
at : Int -> Relation v reachable wrap -> Relation (List v) reachable (Maybe wrap)
at idx =
    makeOneToOne_ ("(" ++ String.fromInt idx ++ ")")
        (if idx < 0 then
            always Nothing

         else
            List.head << List.drop idx
        )
        (\fn ->
            -- NOTE: `<< try` at the end ensures we can't delete any existing keys
            -- so `List.filterMap identity` should be safe
            -- TODO: write this in terms of `foldr` to avoid double iteration.
            List.indexedMap
                (\idx_ v ->
                    if idx == idx_ then
                        fn (Just v)

                    else
                        Just v
                )
                >> List.filterMap identity
        )
        << try


{-| This accessor combinator lets you access Dict members.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Array exposing (Array)
    import Accessors exposing (..)
    import Lens as L

    arr : Array { bar : String }
    arr = Array.fromList [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]

    get (ix 1) arr
    --> Just { bar = "Things" }

    get (ix 9000) arr
    --> Nothing

    get (ix 0 << L.bar) arr
    --> Just "Stuff"

    set (ix 0 << L.bar) "Whatever" arr
    --> Array.fromList [{ bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" }]

    set (ix 9000 << L.bar) "Whatever" arr
    --> arr

-}
ix : Int -> Relation v reachable wrap -> Relation (Array v) reachable (Maybe wrap)
ix idx =
    makeOneToOne_
        ("[" ++ String.fromInt idx ++ "]")
        (Array.get idx)
        (\fn ->
            -- NOTE: `<< try` at the end ensures we can't delete any existing keys
            -- so `List.filterMap identity` should be safe
            -- TODO: there's a better way to write this no doubt.
            Array.indexedMap
                (\idx_ v ->
                    if idx == idx_ then
                        fn (Just v)

                    else
                        Just v
                )
                >> Array.foldl
                    (\e acc ->
                        case e of
                            Just v ->
                                Array.push v acc

                            Nothing ->
                                acc
                    )
                    Array.empty
        )
        << try


{-| Lens over the first component of a Tuple

    import Accessors exposing (..)

    charging : (String, Int)
    charging = ("It's over", 1)

    get one charging
    --> "It's over"

    set one "It's over" charging
    --> ("It's over", 1)

    over one (\s -> String.toUpper s ++ "!!!") charging
    --> ("IT'S OVER!!!", 1)

-}
one : Relation sub reachable wrap -> Relation ( sub, x ) reachable wrap
one =
    makeOneToOne_ "_1" Tuple.first Tuple.mapFirst


{-|

    import Accessors exposing (..)

    meh : (String, Int)
    meh = ("It's over", 1)

    get two meh
    --> 1

    set two 1125 meh
    --> ("It's over", 1125)

    meh
        |> set two 1125
        |> over one (\s -> String.toUpper s ++ "!!!")
        |> over two ((*) 8)
    --> ("IT'S OVER!!!", 9000)

-}
two : Relation sub reachable wrap -> Relation ( x, sub ) reachable wrap
two =
    makeOneToOne_ "_2" Tuple.second Tuple.mapSecond
