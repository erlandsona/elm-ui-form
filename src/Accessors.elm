module Accessors exposing
    ( Relation, Lens, Lens_
    , get, set, over, name
    , makeOneToOne, makeOneToN
    , try, one, two
    ,  Setter
       --, Prism
       -- , c_Just
      , array
      , at
      , at_
        -- , cons
      , id
        -- , iso
      , ix
      , key
      , list

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

import Array exposing (Array)
import Dict exposing (Dict)
import Util.List as ListUtil


{-| Because this is baked on top of Relation... I'm not sold
this is actually a proper "Lens" type signature becuase you can
make Prism-y things with it.
-}
type alias
    Lens
        -- Structure Before action
        s
        -- Structure After action
        t
        -- Focus Before action
        a
        -- Focus After action
        b
    =
    Relation a b t -> Relation s b t


type alias Setter s a wrap =
    Relation a a a -> Relation s a wrap


type alias Lens_ record field =
    Lens record record field field


type alias Prism s t a b =
    Lens s t (Maybe a) (Maybe b)



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
    { get = \a -> a
    , over = \change -> \a -> change a
    , name = ""
    }
        |> Relation


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
    { get = \super -> sub.get (getter super)
    , over = \change super -> mapper (sub.over change) super
    , name = n ++ sub.name
    }
        |> Relation


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
    { get = \super -> getter sub.get super
    , over = \change super -> mapper (sub.over change) super

    -- TODO: This needs to be improved for compositions...
    , name = n ++ sub.name
    }
        |> Relation


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
list : Relation super sub wrap -> Relation (List super) sub (List wrap)
list =
    makeOneToN ":[]" List.map List.map


array : Relation super sub wrap -> Relation (Array super) sub (Array wrap)
array =
    makeOneToN "[]" Array.map Array.map


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
try : Relation sub path wrap -> Relation (Maybe sub) path (Maybe wrap)
try =
    makeOneToN "?" Maybe.map Maybe.map



-- maybe : Relation (Maybe wrap) reachable a -> Relation (Maybe wrap) reachable a
-- maybe =
--     makeOneToOne "^?"
--         (get try)
--         (set try Just)
-- c_Just =
--     makeOneToN "^?"
--         Maybe.map
--         (\fn m -> Maybe.map fn m)


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
key : comparable -> Relation (Maybe v) reachable wrap -> Relation (Dict comparable v) reachable wrap
key k =
    makeOneToOne "{}" (Dict.get k) (Dict.update k)


at_ : Int -> (Relation v reachable wrap -> Relation (List v) reachable (Maybe wrap))
at_ idx =
    makeOneToOne ("[" ++ String.fromInt idx ++ "]?")
        (ListUtil.getIdx idx)
        (\fn ls ->
            List.indexedMap
                (\idx_ v ->
                    if idx == idx_ then
                        fn (Just v)

                    else
                        Just v
                )
                ls
                |> List.filterMap identity
        )
        << try


{-| This accessor combinator lets you access a List member at a given index.

    list = ["foo", "bar", "baz"]

    get (ix 0) list
    -- returns "bar"

    get (ix 3) list
    -- returns Nothing

    get () dict
    -- returns Just 2

    set (dictEntry "foo") Nothing dict
    -- returns Dict.remove "foo" dict

    set (dictEntry "baz" << try << bar) 3 dict
    -- returns dict

-}
at :
    Int
    -> Relation elem value elem
    -> Relation (List elem) value (List elem)
at idx =
    makeOneToN ("(" ++ String.fromInt idx ++ ")")
        (atMap idx)
        (atMap idx)


atMap : Int -> (a -> a) -> List a -> List a
atMap idx fn =
    List.indexedMap
        (\idx_ v ->
            if idx_ == idx then
                fn v

            else
                v
        )


ix :
    Int
    -> Relation elem value elem
    -> Relation (Array elem) value (Array elem)
ix idx =
    makeOneToN
        ("[" ++ String.fromInt idx ++ "]")
        (ixMap_ idx)
        (ixMap_ idx)


ixMap_ : Int -> (a -> a) -> Array a -> Array a
ixMap_ idx fn =
    Array.indexedMap
        (\idx_ v ->
            if idx_ == idx then
                fn v

            else
                v
        )


one : Lens ( a, x ) ( b, x ) a b
one =
    makeOneToOne "_1" Tuple.first Tuple.mapFirst


two : Lens ( x, a ) ( x, b ) a b
two =
    makeOneToOne "_2" Tuple.second Tuple.mapSecond



-- build l val =
--     set l val {{- how do I invent a structure? -}}
-- iso =
--     makeOneToOne "~=" String.toList (\_ -> String.fromList)
