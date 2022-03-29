module Util.Json exposing
    ( Decoder, WithDefaults, MissingDefaults, WithErrors
    , attempt, decode, toTuple, toDecoder
    , withDefault, succeed, maybe
    , unit, value, boolOr, intOr, floatOr, charOr, stringOr
    , list, filteredList, array, dict
    , tuple, triple
    , required, optional
    , map, apply, try, risk, andThen, at, oneOf
    , nullable, doubleEncoded
    , tests
    )

{-| Module for Decoders that always return a valid value.


# Type aliases

@docs Decoder, WithDefaults, MissingDefaults, WithErrors


# Runners

@docs attempt, decode, toTuple, toDecoder


# Constructors

@docs withDefault, succeed, maybe


# Primitives

@docs unit, value, boolOr, intOr, floatOr, charOr, stringOr


# Core Data Structures

@docs list, filteredList, array, dict
@docs tuple, triple


# Records Primitives

@docs required, optional


# Variant Types

@docs factory, variant, nullary, unary, binary, ternary, quaternary, quinary, senary, septenary, octonary


# Functor, Applicative, Monad, Alternative

@docs map, apply, try, risk, andThen, at, oneOf


# Helpers

@docs nullable, doubleEncoded

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Expect
import Json.Decode as Decode exposing (Error)
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Util.Json.Decode as DecodeUtil
import Util.Result as ResultUtil



-- Base


type Decoder status value
    = D (Decode.Decoder (WithErrors value))


type WithDefaults
    = WithDefaults Never


type MissingDefaults
    = MissingDefaults Never


{-| Provided with defaults using the primitives defined in this module
decode will return a value composed of decoded values & defaults paired with
whatever associated errors may have happened along the way.
-}
decode : Decoder WithDefaults value -> Decode.Value -> { errors : List Error, value : value }
decode (D decoder) v =
    Decode.decodeValue decoder v
        |> ResultUtil.extract
            (\_ ->
                -- !!!!!! THIS SHOULD BE IMPOSSIBLE !!!!!!
                -- The only way we can get a Decoder WithDefaults is
                -- if a fallback is provided for every value
                -- SO THIS SHOULD NOT BE ABLE TO FAIL!!!
                decode (D decoder) v
            )


{-| Attempt to decode with a decoder that could potentially fail
-}
attempt : Decoder MissingDefaults value -> Decode.Value -> Result Error { errors : List Error, value : value }
attempt (D dec) =
    Decode.decodeValue dec


{-| In case you need a tuple instead of record accessors :man\_shrugging:
-}
toTuple : { errors : List Error, value : value } -> ( value, List Error )
toTuple i =
    ( i.value, i.errors )


{-| wrap a value in a Decoder
-}
succeed : a -> Decoder WithDefaults a
succeed =
    WithErrors [] >> Decode.succeed >> D


{-| Provide a default value, turning a Decoder that has the potential to fail
due to missing fields or bad data or, into a Decoder that will always succeed.
-}
withDefault : a -> Decode.Decoder a -> Decoder WithDefaults a
withDefault def dec =
    Decode.value
        |> Decode.map
            (\val ->
                case Decode.decodeValue dec val of
                    Ok v ->
                        WithErrors [] v

                    Err e ->
                        WithErrors [ e ] def
            )
        |> D


maybe : Decode.Decoder a -> Decoder WithDefaults (Maybe a)
maybe dec =
    Decode.maybe dec |> withDefault Nothing


toDecoder : Decoder status value -> Decode.Decoder (WithErrors value)
toDecoder (D decoder) =
    decoder


{-| `Decoder` for a unit value, may be useful when responding to
DELETE requests that return HTTP 201 No Content
-}
unit : Decoder WithDefaults ()
unit =
    Decode.null () |> withDefault ()


value : Decoder WithDefaults Encode.Value
value =
    Decode.value |> Decode.map (\v -> WithErrors [] v) |> D


{-| `Decoder` between a JSON boolean and an Elm `Bool`
-}
boolOr : Bool -> Decoder WithDefaults Bool
boolOr def =
    Decode.bool |> withDefault def


{-| `Decoder` between a JSON number and an Elm `Int`
-}
intOr : Int -> Decoder WithDefaults Int
intOr def =
    Decode.int |> withDefault def


{-| `Decoder` between a JSON number and an Elm `Float`
-}
floatOr : Float -> Decoder WithDefaults Float
floatOr def =
    Decode.float |> withDefault def


{-| `Decoder` between a JSON string of length 1 and an Elm `Char`
-}
charOr : Char -> Decoder WithDefaults Char
charOr def =
    (Decode.maybe Decode.string
        |> Decode.andThen
            (\s ->
                case s |> Maybe.andThen String.uncons of
                    Just ( h, "" ) ->
                        Decode.succeed h

                    _ ->
                        Decode.fail "Expecting a CHAR"
            )
    )
        |> withDefault def


{-| `Decoder` between a JSON string and an Elm `String`
-}
stringOr : String -> Decoder WithDefaults String
stringOr def =
    Decode.string
        |> withDefault def



-- DATA STRUCTURES


{-| Swallows errors for elements but probably what you want for most use cases...
-}
filteredList : Decode.Decoder a -> Decoder WithDefaults (List a)
filteredList =
    maybe
        >> list
        >> map (List.filterMap identity)


{-| `Decoder` between a JSON array and an Elm `List`.

Use when a default for the elements is available to fill if decoding fails.
Will still collect errors for the failed decoders

-}
list : Decoder WithDefaults a -> Decoder WithDefaults (List a)
list =
    toDecoder
        >> Decode.list
        >> withDefault []
        >> map
            (List.foldl
                (\elem ( acc, idx ) ->
                    ( WithErrors (acc.errors ++ List.map (Decode.Index idx) elem.errors) (elem.value :: acc.value)
                    , idx + 1
                    )
                )
                ( WithErrors [] [], 0 )
                >> (\( result, _ ) -> WithErrors result.errors (result.value |> List.reverse))
            )
        >> flatten


{-| `Decoder` between a JSON array and an Elm `Array`.
-}
array : Decoder WithDefaults a -> Decoder WithDefaults (Array a)
array =
    toDecoder
        >> Decode.array
        >> withDefault Array.empty
        >> map
            (Array.foldl
                (\elem ( acc, idx ) ->
                    ( WithErrors
                        (acc.errors ++ List.map (Decode.Index idx) elem.errors)
                        (Array.push elem.value acc.value)
                    , idx + 1
                    )
                )
                ( WithErrors [] Array.empty, 0 )
                >> Tuple.first
            )
        >> flatten


{-| `Decoder` between a JSON object and an Elm `Dict`.
-}
dict : Decoder WithDefaults a -> Decoder WithDefaults (Dict String a)
dict =
    toDecoder
        >> Decode.dict
        >> withDefault Dict.empty
        >> map
            (Dict.foldl
                (\key elem acc ->
                    WithErrors
                        (acc.errors ++ List.map (Decode.Field key) elem.errors)
                        (Dict.insert key elem.value acc.value)
                )
                (WithErrors [] Dict.empty)
            )
        >> flatten



-- The infamous "Anonymous Record"'s


{-| `Datum` between a JSON array of length 2 and an Elm `Tuple`.
-}
tuple : Decoder WithDefaults a -> Decoder WithDefaults b -> Decoder WithDefaults ( a, b )
tuple (D d1) (D d2) =
    Decode.map2
        (\r1 r2 -> WithErrors (r1.errors ++ r2.errors) ( r1.value, r2.value ))
        (d1
            |> Decode.andThen
                (\def ->
                    Decode.index 0 d1
                        |> withDefault (WithErrors [] def.value)
                        |> flatten
                        |> toDecoder
                )
        )
        (d2
            |> Decode.andThen
                (\def ->
                    Decode.index 1 d2
                        |> withDefault (WithErrors [] def.value)
                        |> flatten
                        |> toDecoder
                )
        )
        |> D


{-| `Datum` between a JSON array of length 3 and an Elm triple.
-}
triple : Decoder WithDefaults a -> Decoder WithDefaults b -> Decoder WithDefaults c -> Decoder WithDefaults ( a, b, c )
triple (D d1) (D d2) (D d3) =
    Decode.map3
        (\r1 r2 r3 ->
            WithErrors (r1.errors ++ r2.errors ++ r3.errors) ( r1.value, r2.value, r3.value )
        )
        (d1
            |> Decode.andThen
                (\def ->
                    Decode.index 0 d1
                        |> withDefault (WithErrors [] def.value)
                        |> flatten
                        |> toDecoder
                )
        )
        (d2
            |> Decode.andThen
                (\def ->
                    Decode.index 1 d2
                        |> withDefault (WithErrors [] def.value)
                        |> flatten
                        |> toDecoder
                )
        )
        (d3
            |> Decode.andThen
                (\def ->
                    Decode.index 2 d3
                        |> withDefault (WithErrors [] def.value)
                        |> flatten
                        |> toDecoder
                )
        )
        |> D



-- Records


{-| Specify the name, and a `Decoder` for a field.

The name is only used as the field name in the resulting JSON, and has no impact on the Elm side.

-}
required :
    String
    -> List String
    -> Decoder WithDefaults f
    -> Decoder status (f -> b)
    -> Decoder status b
required fst fields decoder =
    andMap
        (at fst
            fields
            (decoder
                |> toDecoder
                |> Decode.map
                    (\with ->
                        if List.isEmpty with.errors then
                            with

                        else
                            WithErrors
                                (List.map (Decode.Field (String.join "." (fst :: fields))) with.errors)
                                with.value
                    )
                |> D
            )
        )


optional :
    String
    -> List String
    -> Decoder WithDefaults f
    -> Decoder status (f -> b)
    -> Decoder status b
optional fst fields decoder =
    andMap
        (at fst fields decoder
            |> toDecoder
            |> Decode.map
                (\old ->
                    case old.errors of
                        (Decode.Failure msg _) :: _ ->
                            if String.startsWith "Expecting an OBJECT with a field named" msg then
                                old.value |> WithErrors []

                            else
                                old

                        _ ->
                            old
                )
            |> D
        )


oneOf : List (Decode.Decoder a) -> a -> Decoder WithDefaults a
oneOf dec def =
    Decode.oneOf dec |> withDefault def


at : String -> List String -> Decoder status a -> Decoder status a
at fst fields decoder =
    List.foldr field decoder (fst :: fields)



-- Transformers


map : (a -> b) -> Decoder WithDefaults a -> Decoder WithDefaults b
map fn (D decoder) =
    Decode.map (\d -> WithErrors d.errors (fn d.value)) decoder
        |> D


{-| andMap for adding a call to `risk`
-}
try : Decoder MissingDefaults a -> Decoder status (a -> b) -> Decoder MissingDefaults b
try =
    andMap


{-| andMap for a `Decoder WithDefaults`
-}
apply : Decoder WithDefaults a -> Decoder status (a -> b) -> Decoder status b
apply =
    andMap


andMap : Decoder any a -> Decoder status (a -> b) -> Decoder other b
andMap (D v) (D fn) =
    Decode.map2 (\d1 d2 -> WithErrors (d1.errors ++ d2.errors) (d2.value d1.value)) v fn
        |> D


{-| By introducing a raw elm/json decoder you're exposing the rest of your decoder
to the possibility of failing
-}
risk : Decode.Decoder a -> Decoder status (a -> b) -> Decoder MissingDefaults b
risk dec (D decoder) =
    Decode.map2 (\d1 d2 -> WithErrors d2.errors (d2.value d1)) dec decoder
        |> D


andThen : (a -> Decoder WithDefaults b) -> Decoder WithDefaults a -> Decoder WithDefaults b
andThen fn =
    toDecoder
        >> Decode.andThen
            (\d ->
                Decode.map
                    (\new -> WithErrors (d.errors ++ new.errors) new.value)
                    (fn d.value |> toDecoder)
            )
        >> D



-- Helpers


nullable : Decoder WithDefaults c -> Decoder WithDefaults c
nullable (D decoder) =
    decoder
        |> Decode.andThen
            (\def ->
                Decode.oneOf [ Decode.null (WithErrors [] def.value), decoder ]
            )
        |> D


doubleEncoded : { hideSensitiveInfo : Bool } -> Decoder WithDefaults a -> Decoder WithDefaults a
doubleEncoded args (D dec) =
    let
        fromString : String -> Decode.Decoder (WithErrors a)
        fromString str =
            case Decode.decodeString dec str of
                Ok v ->
                    Decode.succeed v

                Err error ->
                    let
                        errorStr : String
                        errorStr =
                            if args.hideSensitiveInfo then
                                DecodeUtil.errorToSensitiveString error

                            else
                                Decode.errorToString error
                    in
                    Decode.fail errorStr
    in
    Decode.andThen
        (\def ->
            Decode.string
                |> Decode.andThen fromString
                |> withDefault (WithErrors [] def.value)
                |> flatten
                |> toDecoder
        )
        dec
        |> D



-- Internal Helpers


field : String -> Decoder status a -> Decoder status a
field name (D decoder) =
    decoder
        |> Decode.andThen
            (\dec ->
                Decode.field name decoder
                    |> withDefault (WithErrors [] dec.value)
                    |> flatten
                    |> toDecoder
            )
        |> D


flatten : Decoder WithDefaults (WithErrors a) -> Decoder WithDefaults a
flatten (D decoder) =
    Decode.map
        (\outer ->
            let
                inner : WithErrors a
                inner =
                    outer.value
            in
            WithErrors (inner.errors ++ outer.errors) inner.value
        )
        decoder
        |> D


{-| A record that carries
A. a properly decoded value & [] like `{ value = "Stuff", errors = [] }` or
B. a default value & List Error like `{ value = "", [Decode.Failure "unexpected null value"] }`
-}
type alias WithErrors value =
    { errors : List Error
    , value : value
    }



-- Tests


tests : Test
tests =
    let
        json : Decoder WithDefaults a -> Decoder WithDefaults a
        json =
            doubleEncoded { hideSensitiveInfo = False }
    in
    describe "Util.Json tests"
        [ describe "unit"
            [ test "decodes a null into a ()" <|
                \_ ->
                    WithErrors [] ()
                        |> Expect.equal (decode unit Encode.null)
            ]
        , describe "value"
            [ test "brings in anything as a Encode.Value" <|
                \_ ->
                    WithErrors [] Encode.null
                        |> Expect.equal (decode value Encode.null)
            ]
        , describe "boolOr"
            [ test "decodes a bool" <|
                \_ ->
                    WithErrors [] True
                        |> Expect.equal (decode (boolOr False) (Encode.bool True))
            , test "uses its fallback" <|
                \_ ->
                    WithErrors [ Decode.Failure "Expecting a BOOL" Encode.null ] False
                        |> Expect.equal (decode (boolOr False) Encode.null)
            ]
        , describe "intOr"
            [ test "decodes a int" <|
                \_ ->
                    WithErrors [] 0
                        |> Expect.equal (decode (intOr 47) (Encode.int 0))
            , test "uses its fallback" <|
                \_ ->
                    WithErrors [ Decode.Failure "Expecting an INT" Encode.null ] 0
                        |> Expect.equal (decode (intOr 0) Encode.null)
            ]
        , describe "floatOr"
            [ test "decodes a float" <|
                \_ ->
                    let
                        digits_of_pi_that_I_memorized_as_a_kid : Gloat
                        digits_of_pi_that_I_memorized_as_a_kid =
                            -- elm-format cuts it off? Doesn't it know I'm tryin' to flex here?
                            "3.1415926535897932384626433832795028841"
                                |> String.toFloat
                                |> Maybe.withDefault 2.0
                    in
                    WithErrors [] digits_of_pi_that_I_memorized_as_a_kid
                        |> Expect.equal (decode (floatOr 47) (Encode.float digits_of_pi_that_I_memorized_as_a_kid))
            , test "uses its fallback" <|
                \_ ->
                    WithErrors [ Decode.Failure "Expecting a FLOAT" Encode.null ] 0
                        |> Expect.equal (decode (floatOr 0) Encode.null)
            ]
        , describe "charOr"
            [ test "decodes a char" <|
                \_ ->
                    WithErrors [] 's'
                        |> Expect.equal (decode (charOr 'a') (Encode.string "s"))
            , test "uses its fallback" <|
                \_ ->
                    WithErrors [ Decode.Failure "Expecting a CHAR" Encode.null ] 'a'
                        |> Expect.equal (decode (charOr 'a') Encode.null)
            ]
        , describe "stringOr"
            [ test "decodes a string" <|
                \_ ->
                    WithErrors [] "s"
                        |> Expect.equal (decode (stringOr "a") (Encode.string "s"))
            , test "uses its fallback" <|
                \_ ->
                    WithErrors [ Decode.Failure "Expecting a STRING" Encode.null ] "a"
                        |> Expect.equal (decode (stringOr "a") Encode.null)
            ]
        , describe "list"
            [ test "decodes a list of A's" <|
                \_ ->
                    WithErrors [] [ "s" ]
                        |> Expect.equal (decode (list (stringOr "a")) (Encode.list Encode.string [ "s" ]))
            , test "uses its fallback" <|
                \_ ->
                    WithErrors [ Decode.Failure "Expecting a LIST" Encode.null ] []
                        |> Expect.equal (decode (list (stringOr "a")) Encode.null)
            , test "uses its fallbacks" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.string "[ \"stuff\", 2, true ]"
                    in
                    WithErrors
                        [ Decode.Index 1 (Decode.Failure "Expecting a STRING" (Encode.int 2))
                        , Decode.Index 2 (Decode.Failure "Expecting a STRING" (Encode.bool True))
                        ]
                        [ "stuff", "a", "a" ]
                        |> Expect.equal (decode (json (list (stringOr "a"))) subject)
            ]
        , describe "array"
            [ test "decodes a array of A's" <|
                \_ ->
                    WithErrors [] (Array.fromList [ "s" ])
                        |> Expect.equal (decode (array (stringOr "a")) (Encode.array Encode.string (Array.fromList [ "s" ])))
            , test "uses its fallback" <|
                \_ ->
                    WithErrors [ Decode.Failure "Expecting an ARRAY" Encode.null ] Array.empty
                        |> Expect.equal (decode (array (stringOr "a")) Encode.null)
            , test "uses its fallbacks" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.string "[ \"stuff\", 2, true ]"
                    in
                    WithErrors
                        [ Decode.Index 1 (Decode.Failure "Expecting a STRING" (Encode.int 2))
                        , Decode.Index 2 (Decode.Failure "Expecting a STRING" (Encode.bool True))
                        ]
                        (Array.fromList [ "stuff", "a", "a" ])
                        |> Expect.equal (decode (json (array (stringOr "a"))) subject)
            ]
        , describe "dict"
            [ test "decodes a dict of A's" <|
                \_ ->
                    WithErrors [] (Dict.fromList [ ( "1", "s" ) ])
                        |> Expect.equal (decode (dict (stringOr "a")) (Encode.dict String.fromInt Encode.string (Dict.fromList [ ( 1, "s" ) ])))
            , test "uses its fallback" <|
                \_ ->
                    WithErrors [ Decode.Failure "Expecting an OBJECT" Encode.null ] Dict.empty
                        |> Expect.equal (decode (dict (stringOr "a")) Encode.null)
            , test "uses its fallbacks" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.string "{ \"0\": \"stuff\", \"blah\": 2, \"stuffz\": true }"
                    in
                    WithErrors
                        [ Decode.Field "blah" (Decode.Failure "Expecting a STRING" (Encode.int 2))
                        , Decode.Field "stuffz" (Decode.Failure "Expecting a STRING" (Encode.bool True))
                        ]
                        (Dict.fromList [ ( "0", "stuff" ), ( "blah", "a" ), ( "stuffz", "a" ) ])
                        |> Expect.equal (decode (json (dict (stringOr "a"))) subject)
            ]

        -- , describe ""
        , describe "at"
            [ test "decodes field of record properly" <|
                \_ ->
                    WithErrors [] 1
                        |> Expect.equal (decode (json (at "stuff" [] (intOr 0))) (Encode.string "{\"stuff\": 1}"))
            , test "decodes into a deeply nested object" <|
                \_ ->
                    WithErrors [] 1
                        |> Expect.equal (decode (json (at "stuff" [ "things" ] (intOr 0))) (Encode.string "{\"stuff\": {\"things\": 1}}"))
            , test "falls back on value failure" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.string "{\"stuff\": true}"
                    in
                    WithErrors [ Decode.Failure "Expecting an INT" (Encode.bool True) ] 0
                        |> Expect.equal (decode (json (at "stuff" [] (intOr 0))) subject)
            , test "falls back on field failure" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.string "{\"stff\": 1}"

                        result : WithErrors Int
                        result =
                            decode (json (at "stuff" [] (intOr 0))) subject

                        errStr : String
                        errStr =
                            List.foldl
                                (\elem acc ->
                                    Decode.errorToString elem ++ acc
                                )
                                ""
                                result.errors
                    in
                    ( "Problem with the given value:\n\n{\n        \"stff\": 1\n    }\n\nExpecting an OBJECT with a field named `stuff`", 0 )
                        |> Expect.equal ( errStr, result.value )
            , test "falls back on nested field failure" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.string "{\"stuff\": {\"thngs\": 1}}"

                        result : WithErrors Int
                        result =
                            decode (json (at "stuff" [ "things" ] (intOr 0))) subject

                        errStr : String
                        errStr =
                            List.foldl
                                (\elem acc ->
                                    Decode.errorToString elem ++ acc
                                )
                                ""
                                result.errors
                    in
                    ( "Problem with the given value:\n\n{\n        \"thngs\": 1\n    }\n\nExpecting an OBJECT with a field named `things`", 0 )
                        |> Expect.equal ( errStr, result.value )
            , test "falls back on top level nested field failure" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.string "{\"stff\": {\"things\": 1}}"

                        result : WithErrors Int
                        result =
                            decode (json (at "stuff" [ "things" ] (intOr 0))) subject

                        errStr : String
                        errStr =
                            List.foldl
                                (\elem acc ->
                                    Decode.errorToString elem ++ acc
                                )
                                ""
                                result.errors
                    in
                    ( "Problem with the given value:\n\n{\n        \"stff\": {\n            \"things\": 1\n        }\n    }\n\nExpecting an OBJECT with a field named `stuff`"
                    , 0
                    )
                        |> Expect.equal ( errStr, result.value )
            ]
        , describe "map satisfies Functor laws"
            [ test "fmap id = id" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.int 1
                    in
                    decode (intOr 0 |> map identity) subject
                        |> Expect.equal (decode (intOr 0) subject)
            , test "fmap preserves composition" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.int 1
                    in
                    decode (intOr 0 |> map ((+) 1 >> (+) 1)) subject
                        |> Expect.equal (decode (intOr 0 |> map ((+) 1) |> map ((+) 1)) subject)
            , test "fmap id = id in failure" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.string "blah"
                    in
                    decode (intOr 0 |> map identity) subject
                        |> Expect.equal (decode (intOr 0) subject)
            , test "fmap preserves composition in failure" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.string "blah"
                    in
                    decode (intOr 0 |> map ((+) 1 >> (+) 1)) subject
                        |> Expect.equal (decode (intOr 0 |> map ((+) 1) |> map ((+) 1)) subject)
            ]
        , describe "andMap satisfies applicative laws"
            [ test "Identity" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.int 1
                    in
                    decode (succeed identity |> andMap (intOr 0)) subject
                        |> Expect.equal (decode (intOr 0) subject)
            , test "Identity in failure" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.string "blah"
                    in
                    decode (succeed identity |> andMap (intOr 0)) subject
                        |> Expect.equal (decode (intOr 0) subject)
            , test "Homomorphism" <|
                \_ ->
                    decode (succeed ((+) 1) |> andMap (succeed 0)) Encode.null
                        |> Expect.equal (decode (succeed 1) Encode.null)
            , test "Homomorphism in failure" <|
                \_ ->
                    decode (succeed ((+) 1) |> andMap (succeed 0)) Encode.null
                        |> Expect.equal (decode (succeed 1) Encode.null)
            , test "Interchange: u <*> pure y = pure ($ y) <*> u" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.int 1

                        run : Decoder WithDefaults a -> WithErrors a
                        run f =
                            decode f subject

                        u : Decoder WithDefaults (Int -> Int)
                        u =
                            succeed ((+) 1)

                        y : Int
                        y =
                            0

                        pureY : Decoder WithDefaults Int
                        pureY =
                            succeed y

                        pureApY : Decoder WithDefaults ((Int -> Int) -> Int)
                        pureApY =
                            succeed ((|>) y)
                    in
                    run (andMap pureY u)
                        |> Expect.equal (run (andMap u pureApY))
            , test "composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.int 1

                        run : Decoder WithDefaults a -> WithErrors a
                        run f =
                            decode f subject

                        w : Decoder WithDefaults Int
                        w =
                            intOr 0

                        v : Decoder WithDefaults (Int -> Int)
                        v =
                            succeed ((+) 1)

                        u : Decoder WithDefaults (Int -> Int)
                        u =
                            succeed ((*) 2)

                        comp : Decoder WithDefaults ((a -> b) -> (b -> c) -> (a -> c))
                        comp =
                            succeed (>>)
                    in
                    comp
                        |> andMap v
                        |> andMap u
                        |> andMap w
                        |> run
                        |> Expect.equal
                            (run (andMap (andMap w v) u))
            ]
        , describe "andMap errors"
            [ test "aggregates" <|
                \_ ->
                    let
                        run : Decoder WithDefaults a -> WithErrors a
                        run f =
                            decode f subject

                        subject : Encode.Value
                        subject =
                            Encode.string "stuff"
                    in
                    WithErrors
                        [ Decode.Failure "Expecting a BOOL" subject
                        , Decode.Failure "Expecting an INT" subject
                        ]
                        ( 0, False )
                        |> Expect.equal
                            (succeed Tuple.pair
                                |> andMap (intOr 0)
                                |> andMap (boolOr False)
                                |> run
                            )
            ]
        , describe "andThen satisfies the Monadic laws"
            [ test "left identity: succeed a |> andThen m = m a" <|
                \_ ->
                    let
                        run : Decoder WithDefaults a -> WithErrors a
                        run f =
                            decode f Encode.null
                    in
                    succeed 1
                        |> andThen ((+) 1 >> succeed)
                        |> run
                        |> Expect.equal (((+) 1 >> succeed) 1 |> run)
            , test "right identity: m |> andThen succeed = m" <|
                \_ ->
                    let
                        run : Decoder WithDefaults a -> WithErrors a
                        run f =
                            decode f (Encode.int 1)
                    in
                    intOr 0
                        |> andThen succeed
                        |> run
                        |> Expect.equal (intOr 0 |> run)
            , test "associativity: `m |> andThen g |> andThen h === m |> andThen (\\x -> g x |> andThen h)`" <|
                \_ ->
                    let
                        run : Decoder WithDefaults a -> WithErrors a
                        run f =
                            decode f (Encode.int 1)

                        m : Decoder WithDefaults Int
                        m =
                            intOr 0

                        g : Int -> Decoder WithDefaults Int
                        g =
                            (+) 1 >> succeed

                        h : Int -> Decoder WithDefaults Int
                        h =
                            (*) 2 >> succeed
                    in
                    m
                        |> andThen g
                        |> andThen h
                        |> run
                        |> Expect.equal
                            (m
                                |> andThen
                                    (\x ->
                                        g x |> andThen h
                                    )
                                |> run
                            )
            ]
        , describe "andThen: errors"
            [ test "aggregates" <|
                \_ ->
                    let
                        run : Decoder WithDefaults a -> WithErrors a
                        run f =
                            decode f Encode.null
                    in
                    intOr 0
                        |> andThen (always (stringOr ""))
                        |> andThen (always (boolOr False))
                        |> run
                        |> Expect.equal
                            (WithErrors
                                [ Decode.Failure "Expecting an INT" Encode.null
                                , Decode.Failure "Expecting a STRING" Encode.null
                                , Decode.Failure "Expecting a BOOL" Encode.null
                                ]
                                False
                            )
            ]
        , describe "doubleEncoded"
            [ test "decodes a json object embedded in a json string" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.object
                                [ ( "custom_report"
                                  , Encode.string "{\"primary_color\": {\"red\": 85, \"blue\": 150, \"type\": \"RGB\", \"green\": 12}, \"secondary_color\": {\"red\": 92, \"blue\": 11, \"type\": \"RGB\", \"green\": 91}}"
                                  )
                                ]

                        run : Decoder WithDefaults a -> WithErrors a
                        run f =
                            decode f subject
                    in
                    field "custom_report" (json (at "primary_color" [ "type" ] (stringOr "BUST")))
                        |> run
                        |> Expect.equal (WithErrors [] "RGB")
            , test "fallsback and fails like normal" <|
                \_ ->
                    let
                        subject : Encode.Value
                        subject =
                            Encode.object
                                [ ( "custom_report"
                                    -- , Encode.string "{\"primary_color\": {\"red\": 85, \"blue\": 150, \"type\": \"RGB\", \"green\": 12}, \"secondary_color\": {\"red\": 92, \"blue\": 11, \"type\": \"RGB\", \"green\": 91}}"
                                  , Encode.string "{}"
                                  )
                                ]

                        run : Decoder WithDefaults a -> WithErrors a
                        run f =
                            decode f subject
                    in
                    field "custom_report" (json (at "primary_color" [ "red" ] (stringOr "BUST")))
                        |> run
                        |> Expect.equal
                            (WithErrors
                                [ Decode.Failure "Expecting an OBJECT with a field named `primary_color`" subject ]
                                "BUST"
                            )
            ]
        ]


type alias Gloat =
    Float

