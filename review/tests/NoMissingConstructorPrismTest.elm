module NoMissingConstructorPrismTest exposing (all)

import NoMissingConstructorPrism exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoMissingConstructorPrism"
        [ describe "should generate" shouldGenerate
        , describe "should NOT" dontGenerate
        ]


shouldGenerate : List Test
shouldGenerate =
    [ test "Prism for types with more than one type variable and tuple up arguments given to the same constructor" <|
        \() ->
            """module A exposing (..)

type Data3 a b
    = Data3_Wat a b
    | Data3_Otherwise
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Generating a `c_Data3_Wat` Prism for the type constructor: `Data3_Wat`."
                        , details = [ "missing prism for constructor `Data3_Wat`" ]
                        , under = "Data3_Wat a b"
                        }
                        |> Review.Test.whenFixed
                            """module A exposing (..)

type Data3 a b
    = Data3_Wat a b
    | Data3_Otherwise



c_Data3_Wat : Relation sub sub wrap -> Relation sub sub wrap
c_Data3_Wat =
    makeOneToOne
        "c_Data3_Wat"
        (\\t ->
            case t of
                Data3_Wat a0 a1 ->
                    Just a0

                _ ->
                    Nothing
        )
        (\\fn t ->
            case t of
                Data3_Wat a0 a1 ->
                    Data3_Wat (f a0)

                otherwise ->
                    otherwise
        )"""
                    ]
    , test "Prisms for types with more than one type variable" <|
        \() ->
            """module A exposing (..)



type Data2 a b
    = Data2_Things a
    | Data2_Blah b
    | Data2_Otherwise
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Generating a `c_Data2_Things` Prism for the type constructor: `Data2_Things`."
                        , details = [ "missing prism for constructor `Data2_Things`" ]
                        , under = "Data2_Things a"
                        }
                        |> Review.Test.whenFixed
                            """module A exposing (..)



type Data2 a b
    = Data2_Things a
    | Data2_Blah b
    | Data2_Otherwise



c_Data2_Things : Relation sub sub wrap -> Relation sub sub wrap
c_Data2_Things =
    makeOneToOne
        "c_Data2_Things"
        (\\t ->
            case t of
                Data2_Things a0 ->
                    Just a0

                _ ->
                    Nothing
        )
        (\\fn t ->
            case t of
                Data2_Things a0 ->
                    Data2_Things (f a0)

                otherwise ->
                    otherwise
        )"""
                    , Review.Test.error
                        { message = "Generating a `c_Data2_Blah` Prism for the type constructor: `Data2_Blah`."
                        , details = [ "missing prism for constructor `Data2_Blah`" ]
                        , under = "Data2_Blah b"
                        }
                        |> Review.Test.whenFixed
                            """module A exposing (..)



type Data2 a b
    = Data2_Things a
    | Data2_Blah b
    | Data2_Otherwise



c_Data2_Blah : Relation sub sub wrap -> Relation sub sub wrap
c_Data2_Blah =
    makeOneToOne
        "c_Data2_Blah"
        (\\t ->
            case t of
                Data2_Blah a0 ->
                    Just a0

                _ ->
                    Nothing
        )
        (\\fn t ->
            case t of
                Data2_Blah a0 ->
                    Data2_Blah (f a0)

                otherwise ->
                    otherwise
        )"""
                    ]
    , test "a basic prism" <|
        \() ->
            """module A exposing (..)


type Data1 a
    = Data1_Stuff a
    | Data1_Otherwise
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Generating a `c_Data1_Stuff` Prism for the type constructor: `Data1_Stuff`."
                        , details = [ "missing prism for constructor `Data1_Stuff`" ]
                        , under = "Data1_Stuff a"
                        }
                        |> Review.Test.whenFixed
                            """module A exposing (..)


type Data1 a
    = Data1_Stuff a
    | Data1_Otherwise



c_Data1_Stuff : Relation sub sub wrap -> Relation sub sub wrap
c_Data1_Stuff =
    makeOneToOne
        "c_Data1_Stuff"
        (\\t ->
            case t of
                Data1_Stuff a0 ->
                    Just a0

                _ ->
                    Nothing
        )
        (\\fn t ->
            case t of
                Data1_Stuff a0 ->
                    Data1_Stuff (f a0)

                otherwise ->
                    otherwise
        )"""
                    ]
    ]


dontGenerate : List Test
dontGenerate =
    [ test "attempt to fix Lens-able types" <|
        \() ->
            """module A exposing (..)


type NotAVariant =
    NotAVariant String
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "generate Prism for constructors that already have a prism defined." <|
        \() ->
            """module A exposing (..)

type AlreadyDefined a
    = DontError a
    | Whatever

c_DontError : Prism (AlreadyDefined a) a
c_DontError =
    makeOneToOne "c_DontError"
        (\\t ->
            case t of
                DontError a -> Just a
                otherwise -> Nothing
            )
        (\\fn t ->
            case t of
                DontError a -> DontError (fn a)
                otherwise -> otherwise
        )


"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]
