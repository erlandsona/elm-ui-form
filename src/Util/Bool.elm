module Util.Bool exposing
    ( all
    , any
    , fromString
    , tap
    , toInt
    , toString
    , when
    )


fromString : String -> Maybe Bool
fromString str =
    case String.toLower str of
        "true" ->
            Just True

        "false" ->
            Just False

        _ ->
            Nothing


toString : Bool -> String
toString bool =
    if bool then
        "true"

    else
        "false"


all : List Bool -> Bool
all =
    List.all identity


any : List Bool -> Bool
any =
    List.any identity


when : Bool -> (a -> a) -> a -> a
when pred =
    tap (always pred)


tap : (a -> Bool) -> (a -> a) -> a -> a
tap pred fn data =
    if pred data then
        fn data

    else
        data


toInt : Bool -> Int
toInt bool =
    if bool then
        1

    else
        0
