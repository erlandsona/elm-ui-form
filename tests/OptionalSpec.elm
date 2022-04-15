module OptionalSpec exposing (..)

import Accessors as A exposing (Lens, Relation, Setter)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, char, int, list, string, tuple)
import Gen.Lens as Lens
import Maybe exposing (Maybe)
import String
import Test exposing (..)


eq : a -> a -> Expectation
eq =
    Expect.equal


type StreetType
    = Street
    | Avenue


type Country
    = US
    | UK
    | FI
    | PL
    | DE


type alias Address =
    { streetName : String
    , streetType : StreetType
    , floor : Maybe Int
    , town : String
    , region : Maybe String
    , postcode : String
    , country : Country
    }


type alias Place =
    { name : String
    , description : String
    , address : Maybe Address
    }



-- addressRegionOptional : Optional Address String
-- addressRegionOptional : Relation sub field wrap -> Relation { m | region : Maybe sub } field (Maybe wrap)


addressRegionOptional =
    Lens.region << A.try



-- addressStreetNameLens : Lens Address String
-- addressStreetNameLens =
--     let
--         get a =
--             a.streetName
--         set sn a =
--             { a | streetName = sn }
--     in
--         Lens get set
-- placeAddressOptional : Optional Place Address
-- placeAddressOptional =
--     let
--         getOption p =
--             p.address
--         set a p =
--             { p | address = Just a }
--     in
--         Optional getOption set
-- string2IntPrism : Prism String Int
-- string2IntPrism =
--     Prism String.toInt String.fromInt


string2IntOptional : Relation (Maybe Int) reachable wrap -> Relation String reachable wrap
string2IntOptional =
    A.makeOneToOne
        "string2IntOptional"
        String.toInt
        (\fn ->
            String.toInt
                >> fn
                >> Maybe.map String.fromInt
                >> Maybe.withDefault ""
        )



-- string2CharListIso : Iso String (List Char)
-- string2CharListIso =
--     Iso String.toList String.fromList


addressesWithRegion : Fuzzer Address
addressesWithRegion =
    let
        address name town postcode region =
            { streetName = name
            , streetType = Street
            , floor = Nothing
            , town = town
            , region = Just region
            , postcode = postcode
            , country = US
            }
    in
    Fuzz.map4 address string string string string


addressesWithoutRegion : Fuzzer Address
addressesWithoutRegion =
    let
        address name town postcode =
            { streetName = name, streetType = Street, floor = Nothing, town = town, region = Nothing, postcode = postcode, country = US }
    in
    Fuzz.map3 address string string string


places : Fuzzer Place
places =
    Fuzz.map3 Place string string (Fuzz.maybe addressesWithRegion)


numbers : Fuzzer String
numbers =
    Fuzz.map String.fromInt int


type alias Fun a =
    a -> a


strFun : Fuzzer (Fun String)
strFun =
    Fuzz.oneOf
        -- [ Fuzz.map String.reverse string
        -- , String.toUpper
        -- , String.toLower
        [ Fuzz.map String.append string
        , Fuzz.map (\s -> String.append s >> String.reverse) string
        , Fuzz.map (\s -> String.append s >> String.toUpper) string
        , Fuzz.map (\s -> String.append s >> String.toLower) string
        ]


intFun : Fuzzer (Fun Int)
intFun =
    Fuzz.oneOf
        [ Fuzz.map (+) int
        , Fuzz.map (-) int
        , Fuzz.map (*) int
        , Fuzz.map (//) int
        ]


maybeStrFun : Fuzzer (Fun (Maybe String))
maybeStrFun =
    Fuzz.oneOf
        [ Fuzz.map
            (\_ ->
                Maybe.andThen String.toInt
                    >> Maybe.map String.fromInt
            )
            (Fuzz.maybe string)
        ]


type alias Bob =
    { name : String
    , age : Int
    , email : Maybe String
    , stuff : List String
    }


bobs : Fuzzer Bob
bobs =
    Fuzz.map4 Bob string int (Fuzz.maybe string) (Fuzz.list string)


isSetter : Setter s a wrap -> Fuzzer s -> Fuzzer (Fun a) -> Fuzzer a -> Test
isSetter l fzr fnFzr val =
    describe ("isSetter: " ++ A.name l)
        [ fuzz fzr
            "identity"
            (Expect.true "setter"
                << setter_id l
            )
        , fuzz (Fuzz.tuple3 ( fzr, fnFzr, fnFzr ))
            "composition"
            (\( s, f, g ) ->
                Expect.true "setter" <|
                    setter_composition l s f g
            )
        , fuzz (Fuzz.tuple3 ( fzr, val, val ))
            "set_set"
            (\( s, a, b ) ->
                Expect.true "setter" <|
                    setter_set_set l s a b
            )
        ]


isLens : Setter s a a -> Fuzzer s -> Fuzzer (Fun a) -> Fuzzer a -> Test
isLens l fzr valFn val =
    describe ("isLens: " ++ A.name l)
        [ isSetter l fzr valFn val

        -- There's Traversal laws in here somewhere but not sure they're expressible in Elm.
        , fuzz fzr "lens_set_get" (lens_set_get l >> Expect.true "lens_set_get")
        , fuzz (Fuzz.tuple ( fzr, val ))
            "lens_get_set"
            (\( b, s ) ->
                lens_get_set l b s
                    |> Expect.true "lens_get_set"
            )
        ]


is_try_a_setter : Test
is_try_a_setter =
    describe "Setters"
        [ isLens Lens.name bobs strFun string
        , isLens Lens.age bobs intFun int
        , isSetter (Lens.email << A.try) bobs strFun string
        , isSetter (Lens.stuff << A.at 0) bobs strFun string
        ]


setter_id : Setter super sub wrap -> super -> Bool
setter_id l s =
    A.over l identity s == s


setter_composition :
    -- Eq s =>
    Setter super sub wrap -> super -> Fun sub -> Fun sub -> Bool
setter_composition l s f g =
    A.over l f (A.over l g s) == A.over l (f << g) s


setter_set_set :
    -- Eq s =>
    Setter super sub wrap -> super -> sub -> sub -> Bool
setter_set_set l s a b =
    A.set l b (A.set l a s) == A.set l b s


lens_set_get :
    -- Eq s =>
    Setter super sub sub -> super -> Bool
lens_set_get l s =
    A.set l (A.get l s) s == s


lens_get_set :
    -- Eq a =>
    Setter super sub sub -> super -> sub -> Bool
lens_get_set l s a =
    A.get l (A.set l a s) == a



-- traverse_pure :
--     -- forall f s a. (Applicative f, Eq (f s)) =>
--     LensLike_ f s a -> s -> Bool
-- traverse_pure l s =
--     l pure s == (pure s {- : f s -})
-- traverse_pureMaybe : (Relation sub sub sub -> Relation super sub wrap) -> s -> Expectation
-- traverse_pureMaybe l s =
--     A.over l Just s == (Just s)
-- traverse_pureList :
--     -- Eq s =>
--     (Relation a a a -> Relation s b t) -> s -> Expectation
-- traverse_pureList l s =
--     (l A.id).over List.singleton s == [ s ]
