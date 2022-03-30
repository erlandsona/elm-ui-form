module Util.Maybe exposing
    ( andMap
    , combine
    , combineMap
    , filter
    , firstValue
    , fold
    , ifElse
    , isJust
    , join
    , not
    , orElse
    , present
    , toBool
    , toCmd
    , toList
    , toResult
    , unwrap
    , when
    )

import Task


not : a -> Maybe a -> Maybe a
not pred huh =
    if Just pred == huh then
        Nothing

    else
        Just pred


toResult : e -> Maybe a -> Result e a
toResult err =
    unwrap (Err err) Ok


toCmd : Maybe msg -> Cmd msg
toCmd =
    unwrap Cmd.none (Task.succeed >> Task.perform identity)


orElse : Maybe a -> Maybe a -> Maybe a
orElse snd fst =
    firstValue [ fst, snd ]


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f m =
    case m of
        Just a ->
            f a

        Nothing ->
            default


fold : (a -> b -> b) -> Maybe a -> b -> b
fold f m b =
    case m of
        Just a ->
            f a b

        Nothing ->
            b


ifElse : Maybe a -> (a -> b) -> b -> b
ifElse m f default =
    case m of
        Just a ->
            f a

        Nothing ->
            default


join : Maybe (Maybe a) -> Maybe a
join m =
    case m of
        Just result ->
            result

        _ ->
            Nothing


when : Bool -> a -> Maybe a
when cond value =
    if cond then
        Just value

    else
        Nothing


filter : (a -> Bool) -> Maybe a -> Maybe a
filter cond maybe =
    case maybe of
        Just v ->
            if cond v then
                Just v

            else
                Nothing

        _ ->
            Nothing


toList : Maybe a -> List a
toList maybe =
    case maybe of
        Just v ->
            [ v ]

        Nothing ->
            []


toBool : (a -> Bool) -> Maybe a -> Bool
toBool =
    unwrap False


firstValue : List (Maybe a) -> Maybe a
firstValue maybes =
    case maybes of
        (Just value) :: _ ->
            Just value

        Nothing :: rest ->
            firstValue rest

        [] ->
            Nothing


isJust : Maybe a -> Bool
isJust =
    (/=) Nothing


present : Maybe a -> Bool
present =
    (/=) Nothing


andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap =
    Maybe.map2 (|>)


combine : List (Maybe a) -> Maybe (List a)
combine =
    List.foldr (Maybe.map2 (::)) (Just [])


combineMap : (a -> Maybe b) -> List a -> Maybe (List b)
combineMap f =
    combine << List.map f
