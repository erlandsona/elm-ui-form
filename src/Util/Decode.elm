module Util.Decode exposing
    ( andMap
    , emptyList
    , errorToSensitiveString
    , failOnNothing
    , fromMaybe
    , fromResult
    , fromStringJsonDecoder
    , isNull
    , nullEmptyOrString
    , trimAndNonEmptyString
    )

-- import Expect
-- import Test exposing (Test)

import Json.Decode as Decode exposing (Decoder)
import Util.Maybe as MaybeUtil
import Util.String as StringUtil


emptyList : a -> Decoder a
emptyList v =
    Decode.list (Decode.fail "List is non-empty")
        |> Decode.andThen (\_ -> Decode.succeed v)


{-| This is for those ugly cases where, json needs to be
decoded, that _in itself_ contains a string of other json.
This decoder looks for a string, decodes that, and then
applies the decoder it is given as a parameter, to that
string.
-- Chad, Jan 4 2021
-}
fromStringJsonDecoder : { hideSensitiveInfo : Bool } -> Decoder a -> Decoder a
fromStringJsonDecoder args decoder =
    let
        fromString : String -> Decoder a
        fromString str =
            case Decode.decodeString decoder str of
                Ok value ->
                    Decode.succeed value

                Err error ->
                    let
                        errorStr : String
                        errorStr =
                            if args.hideSensitiveInfo then
                                errorToSensitiveString error

                            else
                                Decode.errorToString error
                    in
                    "String decoder applied and failed with : "
                        ++ errorStr
                        |> Decode.fail
    in
    Decode.string
        |> Decode.andThen fromString


trimAndNonEmptyString : Decoder String
trimAndNonEmptyString =
    Decode.map String.trim Decode.string
        |> Decode.andThen isNonEmpty


isNonEmpty : String -> Decoder String
isNonEmpty str =
    if String.isEmpty str then
        Decode.fail "Empty string"

    else
        Decode.succeed str


nullEmptyOrString : Decoder (Maybe String)
nullEmptyOrString =
    nullOr (Decode.map StringUtil.nonEmpty Decode.string)


nullOr : Decoder (Maybe a) -> Decoder (Maybe a)
nullOr d =
    Decode.oneOf [ Decode.null Nothing, d ]


failOnNothing : Decoder (Maybe a) -> Decoder a
failOnNothing =
    Decode.andThen
        (\maybe ->
            case maybe of
                Just v ->
                    Decode.succeed v

                Nothing ->
                    Decode.fail "Value was Nothing"
        )


isNull : Decoder Bool
isNull =
    [ Decode.null True
    , Decode.succeed False
    ]
        |> Decode.oneOf


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)


{-| JS errors might contain information about the json itself
and much json contains private information of our users that
we dont want to collect. This converts the error to a string
_without_ revealing what the json actually was. The most
revealing information it can contain is the name of a json
field.
-}
errorToSensitiveString : Decode.Error -> String
errorToSensitiveString error =
    case error of
        Decode.Field fieldName subError ->
            [ "Failed to decode under a field named \""
            , fieldName
            , "\" : "
            , errorToSensitiveString subError
            ]
                |> String.concat

        Decode.Index int subError ->
            [ "Failed to decode under index "
            , String.fromInt int
            , " : "
            , errorToSensitiveString subError
            ]
                |> String.concat

        Decode.OneOf errors ->
            [ "Tried these under a OneOf : "
            , List.map errorToSensitiveString errors
                |> String.join ", "
            ]
                |> String.concat

        Decode.Failure subError _ ->
            subError


{-| Transform a result into a decoder
Sometimes it can be useful to use functions that primarily operate on
`Result` in decoders.
import Json.Decode exposing (..)
import Json.Encode
validateString : String -> Result String String
validateString input =
case input of
"" ->
Err "Empty string is not allowed"
\_ ->
Ok input
""" "something" """
|> decodeString (string |> andThen (fromResult << validateString))
--> Ok "something"
""" "" """
|> decodeString (string |> andThen (fromResult << validateString))
--> Err (Failure "Empty string is not allowed" (Json.Encode.string ""))
-}
fromResult : Result String a -> Decoder a
fromResult result =
    case result of
        Ok successValue ->
            Decode.succeed successValue

        Err errorMessage ->
            Decode.fail errorMessage


fromMaybe : String -> Maybe a -> Decoder a
fromMaybe msg =
    MaybeUtil.unwrap (Decode.fail msg) Decode.succeed



---------------------------------------------------------------
-- TESTS --
---------------------------------------------------------------
-- tests : Test
-- tests =
--     Test.describe "Json Decode Util"
--         [ Test.test "Error message for failing to decode an empty string" <|
--             \_ ->
--                 Decode.decodeString (Decode.field "hello" Decode.int) "{}"
--                     |> Result.mapError errorToSensitiveString
--                     |> Expect.equal (Err "Expecting an OBJECT with a field named `hello`")
--         , Test.test "Trim and non empty on empty string" <|
--             \_ ->
--                 Decode.decodeString trimAndNonEmptyString "\"\""
--                     |> Result.mapError Decode.errorToString
--                     |> Expect.equal (Err "Problem with the given value:\n\n\"\"\n\nEmpty string")
--         , Test.test "Trim and non empty on white space" <|
--             \_ ->
--                 Decode.decodeString trimAndNonEmptyString "\"     \""
--                     |> Result.mapError Decode.errorToString
--                     |> Expect.equal (Err "Problem with the given value:\n\n\"     \"\n\nEmpty string")
--         , Test.test "Trim and non empty on non-empty string" <|
--             \_ ->
--                 Decode.decodeString trimAndNonEmptyString "\"a\""
--                     |> Result.mapError Decode.errorToString
--                     |> Expect.equal (Ok "a")
--         ]
