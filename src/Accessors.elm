module Accessors exposing
    ( Relation, Lens, Lens_
    , get, set, over, name
    , makeOneToOne, makeOneToN
    , onEach, try, dictEntry, one, two
    --, Prism
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

@docs Relation, Lens, Lens_, Prism


# Action functions

Action functions are functions that take an accessor and let you perform a
specific action on data using that accessor.

@docs get, set, over, name


# Build accessors

Accessors are built using these functions:

@docs makeOneToOne, makeOneToN


# Common accessors

@docs onEach, try, dictEntry, one, two

-}

import Dict exposing (Dict)


type alias Lens s t a b =
    Relation a b t
    -> Relation s b t


type alias Lens_ record field =
    Lens record record field field



-- Optic sup path wrap sub path wrap
-- type alias Prism sup sub reachable wrap =
--     -- Relation (Maybe sub) sub wrap
--     -- -> Relation super sub wrap
--     -- Relation super sub wrap
--     -- -> Relation (Maybe super) sub (Maybe wrap)
--     Optic sup (Maybe sub)


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


{-| id is a neutral `Relation`. It is used to end a braid of accessors (see
the implementation for get, set and over).
-}
id : Relation a a a
id =
    Relation
        { get = \a -> a
        , over = \change -> \a -> change a
        , name = ""
        }


{-| The get function takes:

  - An accessor,
  - A datastructure with type `super`
    and returns the value accessed by that combinator.

```
get (foo << bar) myRecord
```

-}
get : (Relation sub sub sub -> Relation super sub wrap) -> super -> wrap
get accessor s =
    let
        (Relation relation) =
            accessor id
    in
    relation.get s


{-| This function gives the name of the function as a string...
-}
name : (Relation sub sub sub -> Relation super sub wrap) -> String
name accessor =
    let
        (Relation relation) =
            accessor id
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
set : (Relation sub sub sub -> Relation super sub wrap) -> sub -> super -> super
set accessor value s =
    let
        (Relation relation) =
            accessor id
    in
    relation.over (\_ -> value) s


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
    (Relation sub sub sub -> Relation super sub wrap)
    -> (sub -> sub)
    -> super
    -> super
over accessor change s =
    let
        (Relation relation) =
            accessor id
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
    String
    -> (super -> sub)
    -> ((sub -> sub) -> super -> super)
    -> Relation sub reachable wrap
    -> Relation super reachable wrap
makeOneToOne n getter mapper (Relation sub) =
    Relation
        { get = \super -> sub.get (getter super)
        , over = \change super -> mapper (sub.over change) super
        , name = n ++ sub.name
        }


{-| This function lets you build an accessor for containers that have
a 1:N relation with what they contain, such as `List` (0-N cardinality) or
`Maybe` (0-1). E.g.:

    onEach : Relation elem sub wrap -> Relation (List elem) sub (List wrap)
    onEach =
        makeOneToN
            List.map
            List.map

n.b. implementing those is usually considerably simpler than the type suggests.

-}
makeOneToN :
    String
    -> ((sub -> subWrap) -> super -> superWrap)
    -> ((sub -> sub) -> super -> super)
    -> Relation sub reachable subWrap
    -> Relation super reachable superWrap
makeOneToN n getter mapper (Relation sub) =
    Relation
        { get = \super -> getter sub.get super
        , over = \change super -> mapper (sub.over change) super

        -- TODO: This needs to be improved for compositions...
        , name = n ++ sub.name
        }


{-| This accessor combinator lets you access values inside lists.

    listRecord = { foo = [ {bar = 2}
                         , {bar = 3}
                         , {bar = 4}
                         ]
                 }

    get (foo << onEach << bar) listRecord
    -- returns [2, 3, 4]

    over (foo << onEach << bar) ((+) 1) listRecord
    -- returns {foo = [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
onEach : Relation super sub wrap -> Relation (List super) sub (List wrap)
onEach =
    makeOneToN "[∞]" List.map List.map


{-| This accessor combinator lets you access values inside Maybe.

    maybeRecord = { foo = Just {bar = 2}
                  , qux = Nothing
                  }

    get (foo << try << bar) maybeRecord
    -- returns Just 2

    get (qux << try << bar) maybeRecord
    -- returns Nothing

    over (foo << try << bar) ((+) 1) maybeRecord
    -- returns {foo = Just {bar = 3}, qux = Nothing}

    over (qux << try << bar) ((+) 1) maybeRecord
    -- returns {foo = Just {bar = 2}, qux = Nothing}

-}
try : Relation super sub wrap -> Relation (Maybe super) sub (Maybe wrap)
try =
    makeOneToN "?" Maybe.map Maybe.map


{-| This accessor combinator lets you access Dict members.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    dict = Dict.fromList [("foo", {bar = 2})]

    get (dictEntry "foo") dict
    -- returns Just {bar = 2}

    get (dictEntry "baz" dict)
    -- returns Nothing

    get (dictEntry "foo" << try << bar) dict
    -- returns Just 2

    set (dictEntry "foo") Nothing dict
    -- returns Dict.remove "foo" dict

    set (dictEntry "baz" << try << bar) 3 dict
    -- returns dict

-}
dictEntry : comparable -> Relation (Maybe v) reachable wrap -> Relation (Dict comparable v) reachable wrap
dictEntry key =
    makeOneToOne "{}" (Dict.get key) (Dict.update key)


one : Lens ( a, x ) ( b, x ) a b
one =
    makeOneToOne "_1"
        Tuple.first
        Tuple.mapFirst


two : Lens ( x, a ) ( x, b ) a b
two =
    makeOneToOne "_2"
        Tuple.second
        Tuple.mapSecond
