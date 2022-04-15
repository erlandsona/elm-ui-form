module OptionalSpec exposing (..)

import Accessors as A exposing (Relation, Setter)
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


type alias Bob =
    { name : String
    , age : Int
    }


bobs : Fuzzer Bob
bobs =
    Fuzz.map2 Bob string int


isSetter : Setter s a wrap -> Fuzzer s -> Fuzzer (Fun a) -> Fuzzer a -> Test
isSetter l fzr fnFzr val =
    describe ("isSetter: " ++ A.name l)
        [ fuzz fzr "identity" (setter_id l)
        , fuzz (Fuzz.tuple3 ( fzr, fnFzr, fnFzr ))
            "composition"
            (\( s, f, g ) -> setter_composition l s f g)
        , fuzz (Fuzz.tuple3 ( fzr, val, val ))
            "set_set"
            (\( s, a, b ) -> setter_set_set l s a b)
        ]


is_try_a_setter : Test
is_try_a_setter =
    describe "Setters"
        [ isSetter Lens.name bobs strFun string
        , isSetter Lens.age bobs intFun int
        ]


setter_id : Setter super sub wrap -> super -> Expectation
setter_id l s =
    A.over l identity s |> eq s


setter_composition :
    -- Eq s =>
    Setter super sub wrap -> super -> Fun sub -> Fun sub -> Expectation
setter_composition l s f g =
    A.over l f (A.over l g s) |> eq (A.over l (f << g) s)


setter_set_set :
    -- Eq s =>
    Setter super sub wrap -> super -> sub -> sub -> Expectation
setter_set_set l s a b =
    A.set l b (A.set l a s) |> eq (A.set l b s)


test_optional_property_identity_when_just : Test
test_optional_property_identity_when_just =
    let
        opt =
            addressRegionOptional

        test a =
            A.get opt a |> Maybe.map (\r -> A.set opt r a) |> eq (Just a)
    in
    fuzz addressesWithRegion "test_optional_property_identity_when_just: For some a: A, getOption a |> Maybe.map (r -> set r a)  == Just a" test


test_optional_property_identity_when_nothing : Test
test_optional_property_identity_when_nothing =
    let
        opt =
            addressRegionOptional

        test a =
            A.get opt a |> Maybe.map (\r -> A.set opt r a) |> eq Nothing
    in
    fuzz addressesWithoutRegion "test_optional_property_identity_when_nothing: For some a: A, getOption a |> Maybe.map (r -> set r a)  == Nothing" test



-- test_optional_property_reverse_identity : Test
-- test_optional_property_reverse_identity =
--     let
--         opt =
--             addressRegionOptional
--         test ( a, r ) =
--             A.set opt r a |> A.get opt |> eq (Just r)
--     in
--     fuzz (Fuzz.tuple ( addressesWithoutRegion, string )) "test_optional_property_reverse_identity: For all a: A, set a r |> getOption == Just a" test
-- test_optional_method_compose =
--     let
--         opt =
--             addressRegionOptional << string2IntOptional
--         computed ( a, i ) =
--             A.set opt i a
--         expected ( a, i ) =
--             { a | region = Just (String.fromInt i) }
--     in
--     fuzz (Fuzz.tuple ( addressesWithRegion, int )) "Optional.compose" (\s -> eq (computed s) (expected s))
-- test_optional_method_composeLens =
--     let
--         opt =
--             composeLens addressRegionOptional (fromIso string2CharListIso)
--         computed ( a, cl ) =
--             opt.set cl a
--         expected ( a, cl ) =
--             { a | region = Just (String.fromList cl) }
--     in
--     fuzz (Fuzz.tuple ( addressesWithRegion, list char )) "Optional.composeLens" (\s -> eq (computed s) (expected s))
-- test_lens_method_modifyOption_just =
--     let
--         f sn =
--             String.reverse sn
--         opt =
--             addressRegionOptional
--         computed a =
--             modifyOption opt f a
--         expected a =
--             opt.getOption a |> Maybe.map String.reverse |> Maybe.map (\b -> opt.set b a)
--         test s =
--             eq (computed s) (expected s)
--     in
--     fuzz addressesWithRegion "Optional.modifyOption for Just a" test
-- test_lens_method_modifyOption_nothing =
--     let
--         f sn =
--             String.reverse sn
--         opt =
--             addressRegionOptional
--         computed a =
--             modifyOption opt f a
--         expected a =
--             opt.getOption a |> Maybe.map String.reverse |> Maybe.map (\b -> opt.set b a)
--         test s =
--             eq (computed s) (expected s)
--     in
--     fuzz addressesWithoutRegion "Optional.modifyOption for Nothing" test


test_lens_method_modify_just : Test
test_lens_method_modify_just =
    let
        f sn =
            String.reverse sn

        opt =
            addressRegionOptional

        computed a =
            A.over opt f a

        expected a =
            A.get opt a |> Maybe.map String.reverse |> Maybe.map (\b -> A.set opt b a) |> Maybe.withDefault a

        test s =
            eq (computed s) (expected s)
    in
    fuzz addressesWithRegion "test_lens_method_modify_just: Optional.modify for Just a" test



-- test_optional_method_zip =
--     let
--         address1 =
--             Address "test" Street Nothing "test" Nothing "test" US
--         address2 =
--             Address "test" Street Nothing "test" (Just "test") "test" US
--         opt =
--             zip addressRegionOptional addressRegionOptional
--         computed x =
--             opt.getOption (opt.set x ( address1, address2 ))
--         expected x =
--             Just x
--         test s =
--             eq (computed s) (expected s)
--     in
--     fuzz (Fuzz.tuple ( string, string )) "Optional.zip" test
-- test_optional_method_fromLens =
--     let
--         opt =
--             fromLens addressStreetNameLens
--         computed ( a, s ) =
--             opt.set s a
--         expected ( a, s ) =
--             { a | streetName = s }
--         test s =
--             eq (computed s) (expected s)
--     in
--     fuzz (Fuzz.tuple ( addressesWithRegion, string )) "Optional.fromLens" test
