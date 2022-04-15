module LensLaws exposing (..)

import Accessors as A
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Gen.Lens as Lens
import Test exposing (..)


eq : a -> a -> Expectation
eq =
    Expect.equal


suite : Test
suite =
    describe "Laws for"
        [ let
            lens =
                Lens.name
          in
          describe "generated field lenses"
            [ fuzz string "GetSet" <|
                \v ->
                    A.get lens (A.set lens v { name = "stuff" }) |> eq v
            , fuzz (Fuzz.map (\s -> { name = s }) string) "SetGet" <|
                \s ->
                    A.set lens (A.get lens s) s |> eq s
            , fuzz (Fuzz.map (\s -> { name = s }) string) "SetSet" <|
                \s ->
                    A.set lens "Things" (A.set lens "Bad" s) |> eq (A.set lens "Things" s)
            ]

        -- , let
        --     prism =
        --         A.try
        --   in
        --   describe "prisms"
        --     [ fuzz (Fuzz.maybe string) "GetSet" <|
        --         \a ->
        --             A.set prism (A.get prism a) a |> eq a
        --     ]
        ]
