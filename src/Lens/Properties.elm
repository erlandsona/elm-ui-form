{- # LANGUAGE Rank2Types # -}
{- # LANGUAGE LiberalTypeSynonyms # -}
{- # LANGUAGE ScopedTypeVariables # -}
-- | A collection of properties that can be tested with QuickCheck, to guarantee
-- that you are working with valid 'Lens'es, 'Setter's, 'Traversal's, 'Iso's and
-- 'Prism's.


module Lens.Properties exposing
    ( isIso
    , isLens
    , isPrism
    , isSetter
    , isTraversal
    )

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)



--------------------------------------------------------------------------------
-- | A 'Setter' is only legal if the following 3 laws hold:
--
-- 1. @set l y (set l x a) ≡ set l y a@
--
-- 2. @over l id ≡ id@
--
-- 3. @over l f . over l g ≡ over l (f . g)@


isSetter :
    -- (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Function a) =>
    Setter_ s a -> Expectation
isSetter l =
    Fuzz.oneOf
        [ setter_id l
        , setter_composition l
        , setter_set_set l
        ]



--------------------------------------------------------------------------------
-- | A 'Traversal' is only legal if it is a valid 'Setter' (see 'isSetter' for
-- what makes a 'Setter' valid), and the following laws hold:
--
-- 1. @t pure ≡ pure@
--
-- 2. @fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)@


isTraversal :
    -- (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Function a) =>
    Traversal_ s a -> Property
isTraversal l =
    Fuzz.oneOf
        [ isSetter l
        , traverse_pureMaybe l
        , traverse_pureList l

        -- TODO: Bring back once traverse_compose is monomorphized
        -- do as <- arbitrary
        --        bs <- arbitrary
        --        t <- arbitrary
        --        return $ traverse_compose l (\x -> as++[x]++bs)
        --                                    (\x -> if t then Just x else Nothing)
        ]



--------------------------------------------------------------------------------
-- | A 'Lens' is only legal if it is a valid 'Traversal' (see 'isTraversal' for
-- what this means), and if the following laws hold:
--
-- 1. @get l (set l b a)  ≡ b@
--
-- 2. @set l (get l a) a  ≡ a@
--
-- 3. @set l c (set l b a) ≡ set l c a@


isLens :
    -- (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function a) =>
    Lens_ s a -> Property
isLens l =
    Fuzz.oneOf
        [ lens_set_get l, lens_get_set l, isTraversal l ]



--------------------------------------------------------------------------------


isIso :
    -- (Arbitrary s, Arbitrary a, CoArbitrary s, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function s, Function a) =>
    Iso_ s a -> Property
isIso l =
    Fuzz.oneOf
        [ iso_hither l, iso_yon l, isLens l, isLens (from l) ]



--------------------------------------------------------------------------------


isPrism :
    -- (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function a) =>
    Prism_ s a -> Property
isPrism l =
    Fuzz.oneOf
        [ isTraversal l, prism_yin l, prism_yang l ]



--------------------------------------------------------------------------------
-- The first setter law:


setter_id :
    -- Eq s =>
    Setter_ s a -> s -> Bool
setter_id l s =
    over l id s == s



--  The second setter law:


setter_composition :
    -- Eq s =>
    Setter_ s a -> s -> Fun a a -> Fun a a -> Bool
setter_composition l s (Fun _ f) (Fun _ g) =
    over l f (over l g s) == over l (f << g) s


lens_set_get :
    -- Eq s =>
    Lens_ s a -> s -> Bool
lens_set_get l s =
    set l (get l s) s == s


lens_get_set :
    -- Eq a =>
    Lens_ s a -> s -> a -> Bool
lens_get_set l s a =
    get l (set l a s) == a


setter_set_set :
    -- Eq s =>
    Setter_ s a -> s -> a -> a -> Bool
setter_set_set l s a b =
    set l b (set l a s) == set l b s


iso_hither :
    -- Eq s =>
    AnIso_ s a -> s -> Bool
iso_hither l s =
    (s |> get (cloneIso l << from l)) == s


iso_yon :
    -- Eq a =>
    AnIso_ s a -> a -> Bool
iso_yon l a =
    (a |> get (from l << cloneIso l)) == a


prism_yin :
    -- Eq a =>
    Prism_ s a -> a -> Bool
prism_yin l a =
    preget l (reget l a) == Just a


prism_yang :
    -- Eq s =>
    Prism_ s a -> s -> Bool
prism_yang l s =
    maybe s (reget l) (preget l s) == s


traverse_pure :
    -- forall f s a. (Applicative f, Eq (f s)) =>
    LensLike_ f s a -> s -> Bool
traverse_pure l s =
    l pure s == (pure s {- : f s -})


traverse_pureMaybe :
    -- Eq s =>
    LensLike_ Maybe s a -> s -> Bool
traverse_pureMaybe =
    traverse_pure


traverse_pureList :
    -- Eq s =>
    LensLike_ List s a -> s -> Bool
traverse_pureList =
    traverse_pure



-- TODO: monomorphize RankNTypes here...
-- traverse_compose :-- (Applicative f, Applicative g, Eq (f (g s))) =>
--    Traversal_ s a -> (a -> g a) -> (a -> f a) -> s -> Bool
-- traverse_compose t f g s = (fmap (t f) << t g) s == (getCompose << t (Compose << fmap f << g)) s
