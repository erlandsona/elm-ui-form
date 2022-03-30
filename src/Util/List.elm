module Util.List exposing
    ( atIdx
    , find
    , getElem
    , getIdx
    , groupBy
    , intoPairs
    , keep
    , mapLast
    , mapLastSpecial
    , member
    , memberBy
    , nonEmpty
    , present
    , reject
    , remove
    , removeLastAnd
    , splitWhen
    , takeFirstMatching
    , unique
    , uniqueBy
    )

import Accessors exposing (Relation)


atIdx :
    Int
    -> Relation elem value elem
    -> Relation (List elem) value (List elem)
atIdx idx =
    let
        name : String
        name =
            "[" ++ String.fromInt idx ++ "]"
    in
    Accessors.makeOneToN name (ixMap idx) (ixMap idx)


ixMap : Int -> (a -> a) -> List a -> List a
ixMap idx fn =
    List.indexedMap
        (\idx_ v ->
            if idx_ == idx then
                fn v

            else
                v
        )


nonEmpty : List a -> Maybe (List a)
nonEmpty ls =
    if ls == [] then
        Nothing

    else
        Just ls


{-| Attempts to split the list at the first element where the given predicate is true. If the predicate is not true for any elements in the list, return nothing. Otherwise, return the split list.
splitWhen (\\n -> n == 3) [ 1, 2, 3, 4, 5 ]
--> Just ( [ 1, 2 ], [ 3, 4, 5 ] )
splitWhen (\\n -> n == 6) [ 1, 2, 3, 4, 5 ]
--> Nothing
-}
splitWhen : (a -> Bool) -> List a -> Maybe ( List a, List a )
splitWhen predicate list =
    findIndex predicate list
        |> Maybe.map (\i -> splitAt i list)


{-| Take a number and a list, return a tuple of lists, where first part is prefix of the list of length equal the number, and second part is the remainder of the list. `splitAt n xs` is equivalent to `(take n xs, drop n xs)`.
splitAt 3 [ 1, 2, 3, 4, 5 ]
--> ( [ 1, 2, 3 ], [ 4, 5 ] )
splitAt 1 [ 1, 2, 3 ]
--> ( [ 1 ], [ 2, 3 ] )
splitAt 3 [ 1, 2, 3 ]
--> ( [ 1, 2, 3 ], [] )
splitAt 4 [ 1, 2, 3 ]
--> ( [ 1, 2, 3 ], [] )
splitAt 0 [ 1, 2, 3 ]
--> ( [], [ 1, 2, 3 ] )
splitAt -1 [ 1, 2, 3 ]
--> ( [], [ 1, 2, 3 ] )
-}
splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )


{-| Take a predicate and a list, return the index of the first element that satisfies the predicate. Otherwise, return `Nothing`. Indexing starts from 0.
isEven : Int -> Bool
isEven i =
modBy 2 i == 0
findIndex isEven [ 1, 2, 3 ]
--> Just 1
findIndex isEven [ 1, 3, 5 ]
--> Nothing
findIndex isEven [ 1, 2, 4 ]
--> Just 1
-}
findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex =
    findIndexHelp 0


findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp index predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index

            else
                findIndexHelp (index + 1) predicate xs


{-| Find the first element that satisfies a predicate and return

Just that element. If none match, return Nothing.
find (\\num -> num > 5) [ 2, 4, 6, 8 ]
--> Just 6

-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


{-| Returns `Just` the element at the given index in the list,
or `Nothing` if the index is out of range.
-}
getIdx : Int -> List a -> Maybe a
getIdx idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs


getElem : a -> List a -> Maybe a
getElem elem =
    find ((==) elem)


keep : (a -> Bool) -> List a -> List a
keep =
    List.filter


reject : (a -> Bool) -> List a -> List a
reject pred =
    List.filter (pred >> not)


remove : a -> List a -> List a
remove =
    (==) >> reject


present : List a -> Bool
present =
    not << List.isEmpty


member : a -> List a -> Bool
member a =
    getElem a >> (/=) Nothing


memberBy : (a -> Bool) -> List a -> Bool
memberBy pred =
    find pred >> (/=) Nothing


intoPairs : List a -> ( List ( a, a ), Maybe a )
intoPairs items =
    let
        intoPairsHelp : List a -> List ( a, a ) -> ( List ( a, a ), Maybe a )
        intoPairsHelp remaining pairs =
            case remaining of
                first :: second :: rest ->
                    intoPairsHelp rest (( first, second ) :: pairs)

                first :: [] ->
                    ( pairs, Just first )

                [] ->
                    ( pairs, Nothing )
    in
    intoPairsHelp items []
        |> Tuple.mapFirst List.reverse


{-| just like `List.map` except the map function takes a `Bool` representing
if the element is the last element
-}
mapLastSpecial : (Bool -> before -> after) -> List before -> List after
mapLastSpecial f elems =
    case elems of
        [] ->
            []

        last_ :: [] ->
            [ f True last_ ]

        first :: rest ->
            f False first :: mapLastSpecial f rest


mapLast : (elem -> elem) -> List elem -> List elem
mapLast f list =
    case list of
        [] ->
            []

        first :: [] ->
            [ f first ]

        first :: rest ->
            first :: mapLast f rest


takeFirstMatching : (a -> Bool) -> List a -> ( Maybe a, List a )
takeFirstMatching predicate fullList =
    let
        searchUntil :
            ( Maybe a, List a, List a )
            -> ( Maybe a, List a, List a )
        searchUntil ( _, list, checkedList ) =
            -- check first item in list for condition
            -- move to end of checkedList if condition is not met
            -- stop if condition is met and merge checkedList ++ list
            -- use
            case list of
                [] ->
                    -- out of items to check! done!
                    ( Nothing, [], checkedList )

                x :: xs ->
                    if predicate x then
                        -- we found it! done!
                        ( Just x, xs, checkedList )

                    else
                        -- try again
                        searchUntil ( Nothing, xs, checkedList ++ [ x ] )

        ( mMatched, endOfList, startOfList ) =
            searchUntil ( Nothing, fullList, [] )
    in
    ( mMatched, startOfList ++ endOfList )


removeLastAnd : (a -> Bool) -> List a -> List a
removeLastAnd condition list =
    case List.reverse list of
        [] ->
            []

        x :: xs ->
            if condition x then
                removeLastAnd condition (List.reverse xs)

            else
                List.reverse xs
                    |> removeLastWhile condition


removeLastWhile : (a -> Bool) -> List a -> List a
removeLastWhile condition list =
    case List.reverse list of
        [] ->
            []

        x :: xs ->
            if condition x then
                removeLastWhile condition (List.reverse xs)

            else
                list


{-| Remove duplicate values, keeping the first instance of each element which appears more than once.
unique [ 0, 1, 1, 0, 1 ]
--> [ 0, 1 ]
-}
unique : List a -> List a
unique list =
    uniqueHelp identity [] list []


{-| Drop duplicates where what is considered to be a duplicate is the result of first applying the supplied function to the elements of the list.
-}
uniqueBy : (a -> b) -> List a -> List a
uniqueBy f list =
    uniqueHelp f [] list []


uniqueHelp : (a -> b) -> List b -> List a -> List a -> List a
uniqueHelp f existing remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            let
                computedFirst : b
                computedFirst =
                    f first
            in
            if List.member computedFirst existing then
                uniqueHelp f existing rest accumulator

            else
                uniqueHelp f (computedFirst :: existing) rest (first :: accumulator)


{-| Group a list by a matching property

groupBy .category [ {name="panther",category=Cat}, {name="wolf",category=Dog}, {name="lion",category=Cat} ]
-->
[ (Cat, [{name="Panther",category=Cat},{name="Maine Coon",category=Cat}])
, (Dog, [{name="Poodle",category=Dog}])
]

-}
groupBy : (a -> b) -> List a -> List ( b, List a )
groupBy extract list =
    let
        helper : List a -> List ( b, List a ) -> List ( b, List a )
        helper scattered gathered =
            case scattered of
                [] ->
                    List.reverse gathered

                first :: rest ->
                    let
                        ( matches, remaining ) =
                            List.partition
                                ((\x y -> extract x == extract y) first)
                                rest
                    in
                    helper remaining (( extract first, first :: matches ) :: gathered)
    in
    helper list []



---------------------------------------------------------------
-- TESTS --
---------------------------------------------------------------
-- tests : Test
-- tests =
--     Test.describe "Util.List tests"
--         [ Test.test "`next` on filled list" <|
--             \_ ->
--                 Expect.equal (Next__Elem 2) (next 1 [ 1, 2 ])
--         , Test.test "`next` on filled list with more elements" <|
--             \_ ->
--                 Expect.equal (Next__Elem 3) (next 2 [ 1, 2, 3, 4 ])
--         , Test.test "`next` after last element" <|
--             \_ ->
--                 Expect.equal Next__LastElement (next 2 [ 1, 2 ])
--         , Test.test "`next` on empty list" <|
--             \_ ->
--                 Expect.equal Next__NotMember (next "FIRST" [])
--         , Test.test "`prev` on filled list" <|
--             \_ ->
--                 Expect.equal (Prev__Elem 1) (prev 2 [ 1, 2 ])
--         , Test.test "`prev` on filled list with more elements" <|
--             \_ ->
--                 Expect.equal (Prev__Elem 2) (prev 3 [ 1, 2, 3, 4 ])
--         , Test.test "`prev` before first element" <|
--             \_ ->
--                 Expect.equal Prev__FirstElement (prev 1 [ 1, 2 ])
--         , Test.test "`prev` on empty list" <|
--             \_ ->
--                 Expect.equal Prev__NotMember (prev "LAST" [])
--         ]
