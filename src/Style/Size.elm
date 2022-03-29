module Style.Size exposing
    (  Size
       -- , asFontSize
       -- , asNegativeRem
       -- , asRem

    , asRemStr
    , decrementSize
    , decrementSizeBy
    , fontSizeRem
    , increment
    , isGreaterThan
    , large
    , max
    , medium
    , small
    , toFloat
    , units
    , xlarge
    , xsmall
    , xxsmall
    , zero
    )

-- import Css
--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------


type Size
    = Zero
    | XXSmall
    | XSmall
    | Small
    | Medium
    | Large Int
      -- Generally sizes proceed exponentially, each size is double of the previous unit
      -- Custom is meant to be an exception to that, such as when our designer requests
      -- a size that is not along an exponential scale (2, 4, 8, 16, 32, 64, 128 ..)
      -- Custom however, still is in terms of multiples of a unit size (16px)
      -- - Chad, Sept 18 2020
    | Custom { units : Float }



--------------------------------------------------------------------------------
-- INTERNAL HELPERS
--------------------------------------------------------------------------------
{-
   Those sizes used to be set to map to exact number of pixels: 4, 8, 16, 32 etc.
   Due to RMS-1986 we adjusted those to ~80% which does not make it into a nice pixel grid anymore.
   This applies to all sizes except Large. If that one is adjusted to 80% buttons start to look really bad.
   If we have more time it is probably better ot revert those back to 8x8 grid and adjust individual
   elements for smaller sizes.
-}


toFloat : Size -> Float
toFloat size =
    case size of
        Zero ->
            0

        XXSmall ->
            0.0675

        XSmall ->
            0.125

        Small ->
            0.25

        Medium ->
            0.375

        Large 0 ->
            0.75

        Large n ->
            -- n = 1; 16 * ( 2 ^ 1 ) -> 32
            -- n = 2; 16 * ( 2 ^ 2 ) -> 64
            -- n = 3; 16 * ( 2 ^ 3 ) -> 128
            2 ^ Basics.toFloat n

        Custom param ->
            param.units



--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------


fontSizeRem : Size -> Float
fontSizeRem size =
    case size of
        Zero ->
            0

        XXSmall ->
            0.5

        XSmall ->
            0.75

        Small ->
            0.875

        Medium ->
            1

        Large n ->
            1.125 + (Basics.toFloat n * 0.125)

        Custom params ->
            params.units



-- asFontSize : Size -> Css.Style
-- asFontSize =
--     Css.fontSize << Css.rem << fontSizeRem
-- asRem : Size -> Css.Rem
-- asRem size =
--     Css.rem <| toFloat size
-- asNegativeRem : Size -> Css.Rem
-- asNegativeRem size =
--     Css.rem <| negate <| toFloat size


asRemStr : Size -> String
asRemStr size =
    (String.fromFloat <| toFloat size) ++ "rem"


zero : Size
zero =
    Zero


xxsmall : Size
xxsmall =
    XXSmall


xsmall : Size
xsmall =
    XSmall


small : Size
small =
    Small


medium : Size
medium =
    Medium


large : Size
large =
    Large 0


xlarge : Int -> Size
xlarge =
    Large


units : Float -> Size
units fl =
    Custom { units = fl }


max : Size -> Size -> Size
max s t =
    if isGreaterThan t s then
        s

    else
        t


isGreaterThan : Size -> Size -> Bool
isGreaterThan s t =
    toFloat t > toFloat s


increment : Size -> Size
increment size =
    case size of
        Zero ->
            XXSmall

        XXSmall ->
            XSmall

        XSmall ->
            Small

        Small ->
            Medium

        Medium ->
            Large 0

        Large n ->
            Large (n + 1)

        Custom param ->
            Custom { units = param.units * 2 }


decrementSizeBy : Int -> Size -> Size
decrementSizeBy i size =
    if i > 0 then
        decrementSizeBy (i - 1) <| decrementSize size

    else
        size


decrementSize : Size -> Size
decrementSize size =
    case size of
        Zero ->
            Zero

        XXSmall ->
            Zero

        XSmall ->
            XXSmall

        Small ->
            XSmall

        Medium ->
            Small

        Large 0 ->
            Medium

        Large n ->
            Large <| Basics.max 0 (n - 1)

        Custom param ->
            Custom { units = Basics.max 0 (param.units / 2) }
