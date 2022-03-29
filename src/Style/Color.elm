module Style.Color exposing
    ( Color
    , DevApi
    , Value
    , black
    , blackValue
    , blue
    , blueValue
    , brightGreen
    , brightGreenValue
    , contrastingFontColor_
    , darkBlue
    , darken
    , darken_
    , devApi
    , dullWhite
    , dullWhiteValue
    , gray0
    , gray1
    , gray2
    , gray3
    , gray4
    , grayValue0
    , grayValue1
    , grayValue2
    , grayValue3
    , grayValue4
    , green
    , greenValue
    , increment
    , lightBlue
    , lightBlueValue
    , lighten_
    , primary
    , primary__contrast
    , red
    , redValue
    , secondary
    , secondary__contrast
    , secondary__contrastToned6
    , secondary__toned
    , secondary__toned2
    , secondary__toned4
    , themeColorDecoder
    , toHexStr
    , toHexStr_
    , toName
    , toString
    , toUI
    , toneForContrast
    , toneForContrast_
    , value
    , verityBlack
    , white
    , whiteValue
    , yellow
    )

import Element.WithContext as Element
import Json.Decode as Decode
import Util.Json as Json exposing (Decoder, WithDefaults)



---------------------------------------------------------------
-- TYPES --
---------------------------------------------------------------


type Color
    = Value Value
    | Variable Variable


type Value
    = Blue Degree3
    | Red
    | White
    | Green Degree2
    | Gray Degree5
    | Yellow
    | Black
      -- These `Float` in `Lighten` and `Darken`
      -- Are the extent to which it should lighten
      -- or darken. When `1`, `Lighten` and `Darken`
      -- will output 100% white or black respectively.
      -- When `0` `Lighten` and `Darken` wont change the
      -- color at all
      --
      -- - Chad Apr 7 2021
    | Lighten Float Value
    | Darken Float Value
    | Theme Theme_


type alias Theme_ =
    { red : Int
    , green : Int
    , blue : Int
    }


type Variable
    = Variable__Primary Variant
    | Variable__Secondary Variant


type Variant
    = Variant__Base
    | Variant__BaseToned
    | Variant__BaseToned2
    | Variant__BaseToned4
    | Variant__Contrast
    | Variant__ContrastToned6


primary : Color
primary =
    Variable <| Variable__Primary Variant__Base


primary__contrast : Color
primary__contrast =
    Variable <| Variable__Primary Variant__Contrast


secondary : Color
secondary =
    Variable <| Variable__Secondary Variant__Base


secondary__contrast : Color
secondary__contrast =
    Variable <| Variable__Secondary Variant__Contrast


secondary__toned : Color
secondary__toned =
    Variable <| Variable__Secondary Variant__BaseToned


secondary__toned2 : Color
secondary__toned2 =
    Variable <| Variable__Secondary Variant__BaseToned2


secondary__toned4 : Color
secondary__toned4 =
    Variable <| Variable__Secondary Variant__BaseToned4


secondary__contrastToned6 : Color
secondary__contrastToned6 =
    Variable <| Variable__Secondary Variant__ContrastToned6


{-| The degree is a some what ambiguous way of describing
the intensity of the color. This is ambiguous because
the degrees of intensity are not a simple and standard
step up in brightness. Rather, each color needs to be
handled on a case by case basis, changing in hue,
saturation, and brightness to various degrees.
-}
type Degree2
    = Degree2__High
    | Degree2__Low


type Degree3
    = Degree3__Low
    | Degree3__Medium
    | Degree3__High


type Degree5
    = Degree5__VeryLow
    | Degree5__Low
    | Degree5__Medium
    | Degree5__High
    | Degree5__VeryHigh


type alias Rgba =
    { r : Int
    , g : Int
    , b : Int
    , a : Float
    }



---------------------------------------------------------------
-- Internal Helpers --
---------------------------------------------------------------
-- colorFromStr : String -> Css.Color
-- colorFromStr str =
--     let
--         c : Css.Color
--         c =
--             Css.rgb 0 0 0
--     in
--     { c | value = str }


contrastRatio : Value -> Value -> Float
contrastRatio c1 c2 =
    -- https://www.w3.org/TR/WCAG20/#contrast-ratiodef
    let
        luminance1 : Float
        luminance1 =
            toLuminance c1

        luminance2 : Float
        luminance2 =
            toLuminance c2
    in
    if luminance1 > luminance2 then
        (luminance1 + 0.05) / (luminance2 + 0.05)

    else
        (luminance2 + 0.05) / (luminance1 + 0.05)


toLuminance : Value -> Float
toLuminance c =
    -- https://www.w3.org/TR/WCAG20/#relativeluminancedef
    let
        rgba8bit : Rgba
        rgba8bit =
            toRgba c

        toSRGB : Int -> Float
        toSRGB colorChannel8bit =
            Basics.toFloat colorChannel8bit / 255

        c_ : Float -> Float
        c_ sRGB =
            if sRGB <= 0.03928 then
                sRGB / 12.92

            else
                ((sRGB + 0.055) / 1.055) ^ 2.4
    in
    List.foldl (+)
        0
        [ (.r rgba8bit |> toSRGB |> c_) * 0.2126
        , (.g rgba8bit |> toSRGB |> c_) * 0.7152
        , (.b rgba8bit |> toSRGB |> c_) * 0.0722
        ]


toRgba : Value -> Rgba
toRgba color =
    let
        rgb : Int -> Int -> Int -> Rgba
        rgb r g b =
            Rgba r g b 1

        noHue : Int -> Rgba
        noHue i =
            rgb i i i
    in
    case color of
        Blue degree ->
            case degree of
                Degree3__High ->
                    rgb 35 105 210

                Degree3__Medium ->
                    rgb 37 63 130

                Degree3__Low ->
                    rgb 25 44 90

        White ->
            noHue 255

        Gray degree ->
            case degree of
                Degree5__VeryHigh ->
                    noHue 238

                Degree5__High ->
                    noHue 217

                Degree5__Medium ->
                    noHue 196

                Degree5__Low ->
                    noHue 144

                Degree5__VeryLow ->
                    noHue 103

        Red ->
            rgb 244 15 126

        Green degree ->
            case degree of
                Degree2__High ->
                    rgb 112 213 156

                Degree2__Low ->
                    rgb 85 155 28

        Black ->
            noHue 17

        Lighten tintAlpha wrappedColor ->
            blend tintAlpha White wrappedColor

        Darken tintAlpha wrappedColor ->
            blend tintAlpha Black wrappedColor

        Theme rgb_ ->
            { r = rgb_.red
            , g = rgb_.green
            , b = rgb_.blue
            , a = 1
            }

        Yellow ->
            rgb 255 255 0


white_ : Element.Color
white_ =
    { red = 1
    , green = 1
    , blue = 1
    , alpha = 1
    }
        |> Element.fromRgb


black_ : Element.Color
black_ =
    { red = 0
    , green = 0
    , blue = 0
    , alpha = 1
    }
        |> Element.fromRgb


darken_ : Element.Color -> Element.Color
darken_ =
    blend_ 0.04 black_


lighten_ : Element.Color -> Element.Color
lighten_ =
    blend_ 0.1 white_


blend_ : Float -> Element.Color -> Element.Color -> Element.Color
blend_ tintAlpha srcColor dstColor =
    let
        dst : { red : Float, green : Float, blue : Float, alpha : Float }
        dst =
            let
                d : { red : Float, green : Float, blue : Float, alpha : Float }
                d =
                    Element.toRgb dstColor
            in
            { red = d.red * 255, green = d.green * 255, blue = d.blue * 255, alpha = d.alpha }

        src : { red : Float, green : Float, blue : Float, alpha : Float }
        src =
            let
                s : { red : Float, green : Float, blue : Float, alpha : Float }
                s =
                    Element.toRgb srcColor
            in
            { red = s.red * 255, green = s.green * 255, blue = s.blue * 255, alpha = s.alpha }

        blendChannel : Float -> Float -> Float
        blendChannel channelS channelD =
            ((channelS * tintAlpha) + (channelD * dst.alpha * (1 - tintAlpha)))
                / aOut

        aOut : Float
        aOut =
            tintAlpha + dst.alpha * (1 - tintAlpha)
    in
    { red =
        blendChannel src.red dst.red
            |> round
            |> clamp 0 255
    , green =
        blendChannel src.green dst.green
            |> round
            |> clamp 0 255
    , blue =
        blendChannel src.blue dst.blue
            |> round
            |> clamp 0 255
    , alpha = aOut
    }
        |> Element.fromRgb255


blend : Float -> Value -> Value -> Rgba
blend tintAlpha srcColor dstColor =
    -- https://en.wikipedia.org/wiki/Alpha_compositing#Alpha_blending
    let
        dst : Rgba
        dst =
            toRgba dstColor

        src : Rgba
        src =
            toRgba srcColor

        blendChannel : Int -> Int -> Int
        blendChannel channelS channelD =
            (((toFloat channelS * tintAlpha) + (toFloat channelD * dst.a * (1 - tintAlpha)))
                / aOut
            )
                |> round

        aOut : Float
        aOut =
            tintAlpha + dst.a * (1 - tintAlpha)
    in
    { r = blendChannel src.r dst.r
    , g = blendChannel src.g dst.g
    , b = blendChannel src.b dst.b
    , a = aOut
    }


asVariant : Value -> Variant -> Value
asVariant val variant =
    case variant of
        Variant__Base ->
            val

        Variant__BaseToned ->
            val
                |> toneValueForContrast

        Variant__BaseToned2 ->
            val
                |> toneValueForContrast
                |> toneValueForContrast

        Variant__BaseToned4 ->
            val
                |> toneValueForContrast
                |> toneValueForContrast
                |> toneValueForContrast
                |> toneValueForContrast

        Variant__Contrast ->
            val
                |> contrastingFontColor

        Variant__ContrastToned6 ->
            val
                |> contrastingFontColor
                |> toneValueForContrast
                |> toneValueForContrast
                |> toneValueForContrast
                |> toneValueForContrast
                |> toneValueForContrast
                |> toneValueForContrast


allVariants : List Variant
allVariants =
    [ Variant__Base
    , Variant__BaseToned
    , Variant__BaseToned2
    , Variant__BaseToned4
    , Variant__Contrast
    , Variant__ContrastToned6
    ]


decrementDegree3 : Degree3 -> Degree3
decrementDegree3 degree =
    case degree of
        Degree3__Low ->
            Degree3__Low

        Degree3__Medium ->
            Degree3__Low

        Degree3__High ->
            Degree3__Medium


incrementDegree2 : Degree2 -> Degree2
incrementDegree2 _ =
    Degree2__High


incrementDegree3 : Degree3 -> Degree3
incrementDegree3 degree =
    case degree of
        Degree3__High ->
            Degree3__High

        Degree3__Medium ->
            Degree3__High

        Degree3__Low ->
            Degree3__Medium


incrementDegree5 : Degree5 -> Degree5
incrementDegree5 degree =
    case degree of
        Degree5__VeryHigh ->
            Degree5__VeryHigh

        Degree5__High ->
            Degree5__VeryHigh

        Degree5__Medium ->
            Degree5__High

        Degree5__Low ->
            Degree5__Medium

        Degree5__VeryLow ->
            Degree5__Low


blueDegree : Degree3
blueDegree =
    Degree3__Medium


greenDegree : Degree2
greenDegree =
    Degree2__Low



---------------------------------------------------------------
-- API --
---------------------------------------------------------------
-- allCssVariables :
--     { primary : Value
--     , secondary : Value
--     }
--     -> Css.Style
-- allCssVariables themeColors =
--     let
--         toCssVariable_ : Value -> Variable -> Css.Style
--         toCssVariable_ val variable =
--             Css.property
--                 (variableToString variable)
--                 (toHexStr val)
--     in
--     [ allVariants
--         |> List.map
--             (\variant ->
--                 toCssVariable_
--                     (asVariant themeColors.primary variant)
--                     (Variable__Primary variant)
--             )
--     , allVariants
--         |> List.map
--             (\variant ->
--                 toCssVariable_
--                     (asVariant themeColors.secondary variant)
--                     (Variable__Secondary variant)
--             )
--     ]
--         |> List.concat
--         |> Css.batch


contrastingFontColor : Value -> Value
contrastingFontColor c =
    let
        contrastRatioWhite : Float
        contrastRatioWhite =
            contrastRatio c White

        contrastRatioBlack : Float
        contrastRatioBlack =
            contrastRatio c Black
    in
    if contrastRatioWhite > contrastRatioBlack then
        White

    else
        Black


increment : Color -> Color
increment sc =
    case sc of
        Variable v ->
            Variable <| toneForContrastVariable v

        Value c ->
            Value <|
                case c of
                    Blue degree3 ->
                        Blue (incrementDegree3 degree3)

                    Red ->
                        Red

                    White ->
                        White

                    Green degree2 ->
                        Green (incrementDegree2 degree2)

                    Gray degree4 ->
                        Gray (incrementDegree5 degree4)

                    Black ->
                        Black

                    Lighten tintAlpha wrappedColor ->
                        Lighten (tintAlpha + ((1 - tintAlpha) / 2)) wrappedColor

                    Darken tintAlpha wrappedColor ->
                        Darken (tintAlpha / 2) wrappedColor

                    Theme _ ->
                        c

                    Yellow ->
                        c


toName : Color -> String
toName sc =
    case sc of
        Value c ->
            case c of
                Blue degree3 ->
                    case degree3 of
                        Degree3__Low ->
                            "Dark Blue"

                        Degree3__Medium ->
                            "Blue"

                        Degree3__High ->
                            "Light Blue"

                Red ->
                    "Red"

                White ->
                    "White"

                Green degree2 ->
                    case degree2 of
                        Degree2__High ->
                            "Bright Green"

                        Degree2__Low ->
                            "Green"

                Gray degree4 ->
                    case degree4 of
                        Degree5__VeryLow ->
                            "Gray 0"

                        Degree5__Low ->
                            "Gray 1"

                        Degree5__Medium ->
                            "Gray 2"

                        Degree5__High ->
                            "Gray 3"

                        Degree5__VeryHigh ->
                            "Gray 4"

                Black ->
                    "Black"

                Lighten _ _ ->
                    "Custom"

                Darken _ _ ->
                    "Custom"

                Theme _ ->
                    "Custom"

                Yellow ->
                    "Yellow"

        Variable v ->
            "Css Variable: " ++ variableToString v


yellow : Color
yellow =
    Value Yellow


verityBlack : Value
verityBlack =
    Theme { red = 16, green = 24, blue = 32 }


black : Color
black =
    Value Black


blackValue : Value
blackValue =
    Black


greenValue : Value
greenValue =
    Green Degree2__Low


brightGreenValue : Value
brightGreenValue =
    Green Degree2__High


darken : Color -> Color
darken sc =
    case sc of
        Value c ->
            Value <| Darken 0.04 c

        Variable _ ->
            sc


toneForContrastVariable : Variable -> Variable
toneForContrastVariable var =
    let
        toneVariant : Variant -> Variant
        toneVariant variant =
            case variant of
                Variant__Base ->
                    Variant__BaseToned

                Variant__BaseToned ->
                    Variant__BaseToned2

                Variant__BaseToned2 ->
                    Variant__BaseToned4

                Variant__BaseToned4 ->
                    Variant__BaseToned4

                Variant__Contrast ->
                    Variant__ContrastToned6

                Variant__ContrastToned6 ->
                    Variant__ContrastToned6
    in
    case var of
        Variable__Primary variant ->
            Variable__Primary (toneVariant variant)

        Variable__Secondary variant ->
            Variable__Secondary (toneVariant variant)


toneForContrast : Color -> Color
toneForContrast sc =
    case sc of
        Value c ->
            Value <| toneValueForContrast c

        Variable v ->
            toneForContrastVariable v
                |> Variable


toneForContrast_ : Element.Color -> Element.Color
toneForContrast_ c =
    let
        contrastRatioWhite : Float
        contrastRatioWhite =
            contrastRatio_ c white_

        contrastRatioBlack : Float
        contrastRatioBlack =
            contrastRatio_ c black_
    in
    if contrastRatioWhite > contrastRatioBlack then
        blend_ 0.04 white_ c

    else
        blend_ 0.04 black_ c


contrastingFontColor_ : Element.Color -> Element.Color
contrastingFontColor_ c =
    let
        contrastRatioWhite : Float
        contrastRatioWhite =
            contrastRatio_ c white_

        contrastRatioBlack : Float
        contrastRatioBlack =
            contrastRatio_ c black_
    in
    if contrastRatioWhite > contrastRatioBlack then
        white_

    else
        black_


contrastRatio_ : Element.Color -> Element.Color -> Float
contrastRatio_ c1 c2 =
    -- https://www.w3.org/TR/WCAG20/#contrast-ratiodef
    let
        luminance1 : Float
        luminance1 =
            toLuminance_ c1

        luminance2 : Float
        luminance2 =
            toLuminance_ c2
    in
    if luminance1 > luminance2 then
        (luminance1 + 0.05) / (luminance2 + 0.05)

    else
        (luminance2 + 0.05) / (luminance1 + 0.05)


toLuminance_ : Element.Color -> Float
toLuminance_ color =
    -- https://www.w3.org/TR/WCAG20/#relativeluminancedef
    let
        c :
            { red : Float
            , green : Float
            , blue : Float
            , alpha : Float
            }
        c =
            Element.toRgb color

        c_ : Float -> Float
        c_ sRGB =
            if sRGB <= 0.03928 then
                sRGB / 12.92

            else
                ((sRGB + 0.055) / 1.055) ^ 2.4
    in
    List.foldl (+)
        0
        [ (c.red |> c_) * 0.2126
        , (c.green |> c_) * 0.7152
        , (c.blue |> c_) * 0.0722
        ]


toneValueForContrast : Value -> Value
toneValueForContrast val =
    let
        contrastRatioWhite : Float
        contrastRatioWhite =
            contrastRatio val White

        contrastRatioBlack : Float
        contrastRatioBlack =
            contrastRatio val Black
    in
    if contrastRatioWhite > contrastRatioBlack then
        Lighten 0.04 val

    else
        Darken 0.04 val


brightGreen : Color
brightGreen =
    Value <| Green <| incrementDegree2 greenDegree


green : Color
green =
    Value <| Green greenDegree


blue : Color
blue =
    Value blueValue


blueValue : Value
blueValue =
    Blue blueDegree


lightBlue : Color
lightBlue =
    Value lightBlueValue


lightBlueValue : Value
lightBlueValue =
    Blue <| incrementDegree3 blueDegree


red : Color
red =
    Value <| Red


darkBlue : Color
darkBlue =
    Value <| Blue <| decrementDegree3 blueDegree


white : Color
white =
    Value <| White


dullWhite : Color
dullWhite =
    Value dullWhiteValue


gray0 : Color
gray0 =
    Value grayValue0


gray1 : Color
gray1 =
    Value grayValue1


gray2 : Color
gray2 =
    Value grayValue2


gray3 : Color
gray3 =
    Value grayValue3


gray4 : Color
gray4 =
    Value grayValue4


redValue : Value
redValue =
    Red


whiteValue : Value
whiteValue =
    White


grayValue0 : Value
grayValue0 =
    Gray Degree5__VeryLow


grayValue1 : Value
grayValue1 =
    Gray Degree5__Low


grayValue2 : Value
grayValue2 =
    Gray Degree5__Medium


grayValue3 : Value
grayValue3 =
    Gray Degree5__High


grayValue4 : Value
grayValue4 =
    Gray Degree5__VeryHigh


dullWhiteValue : Value
dullWhiteValue =
    Darken 0.04 White


variableToString : Variable -> String
variableToString variable_ =
    let
        variantModifier : Variant -> String
        variantModifier variant =
            case variant of
                Variant__Base ->
                    ""

                Variant__BaseToned ->
                    "--toned"

                Variant__BaseToned2 ->
                    "--toned2"

                Variant__BaseToned4 ->
                    "--toned4"

                Variant__Contrast ->
                    "--contrast"

                Variant__ContrastToned6 ->
                    "--contrast-toned4"
    in
    case variable_ of
        Variable__Primary variant ->
            "--primary-color" ++ variantModifier variant

        Variable__Secondary variant ->
            "--secondary-color" ++ variantModifier variant


toCssVariable : Variable -> String
toCssVariable v =
    "var(" ++ variableToString v ++ ")"



-- toCssColor : Color -> Css.Color
-- toCssColor sc =
--     case sc of
--         Value c ->
--             let
--                 { r, g, b, a } =
--                     toRgba c
--             in
--             Css.rgba r g b a
--         Variable v ->
--             colorFromStr <| toCssVariable v


value : Value -> Color
value =
    Value


toUI : Value -> Element.Color
toUI =
    toRgba
        >> (\{ r, g, b, a } ->
                Element.rgba255 r g b a
           )


toString : Color -> String
toString sc =
    case sc of
        Value c ->
            toHexStr c

        Variable v ->
            toCssVariable v


toHexStr_ : Element.Color -> String
toHexStr_ =
    Element.toRgb
        >> (\c ->
                let
                    alphaInt : Int
                    alphaInt =
                        round (c.alpha * 255)

                    unsafeInt255Digits : Int -> ( Int, Int )
                    unsafeInt255Digits n =
                        let
                            digit1 : Int
                            digit1 =
                                n // 16

                            digit0 : Int
                            digit0 =
                                if digit1 /= 0 then
                                    modBy (digit1 * 16) n

                                else
                                    n
                        in
                        ( digit1, digit0 )

                    unsafeIntToChar : Int -> Char
                    unsafeIntToChar i =
                        if i < 10 then
                            String.fromInt i
                                |> String.uncons
                                |> Maybe.map Tuple.first
                                |> Maybe.withDefault '0'

                        else
                            case i of
                                10 ->
                                    'a'

                                11 ->
                                    'b'

                                12 ->
                                    'c'

                                13 ->
                                    'd'

                                14 ->
                                    'e'

                                15 ->
                                    'f'

                                _ ->
                                    '0'

                    hexStr : Int -> String
                    hexStr i =
                        if i < 0 then
                            "00"

                        else if i > 255 then
                            "ff"

                        else
                            unsafeInt255Digits i
                                |> Tuple.mapBoth unsafeIntToChar unsafeIntToChar
                                |> (\( c_, d ) -> String.cons c_ (String.cons d ""))
                in
                String.concat
                    [ "#"
                    , hexStr (c.red * 255 |> floor)
                    , hexStr (c.green * 255 |> floor)
                    , hexStr (c.blue * 255 |> floor)
                    , hexStr alphaInt
                    ]
           )


toHexStr : Value -> String
toHexStr c =
    let
        { r, g, b, a } =
            toRgba c
    in
    let
        alphaInt : Int
        alphaInt =
            round (a * 255)

        unsafeInt255Digits : Int -> ( Int, Int )
        unsafeInt255Digits n =
            let
                digit1 : Int
                digit1 =
                    n // 16

                digit0 : Int
                digit0 =
                    if digit1 /= 0 then
                        modBy (digit1 * 16) n

                    else
                        n
            in
            ( digit1, digit0 )

        unsafeIntToChar : Int -> Char
        unsafeIntToChar i =
            if i < 10 then
                String.fromInt i
                    |> String.uncons
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault '0'

            else
                case i of
                    10 ->
                        'a'

                    11 ->
                        'b'

                    12 ->
                        'c'

                    13 ->
                        'd'

                    14 ->
                        'e'

                    15 ->
                        'f'

                    _ ->
                        '0'

        hexStr : Int -> String
        hexStr i =
            if i < 0 then
                "00"

            else if i > 255 then
                "ff"

            else
                unsafeInt255Digits i
                    |> Tuple.mapBoth unsafeIntToChar unsafeIntToChar
                    |> (\( c_, d ) -> String.cons c_ (String.cons d ""))
    in
    String.concat
        [ "#"
        , hexStr r
        , hexStr g
        , hexStr b
        , hexStr alphaInt
        ]


themeColorDecoder : Value -> Decoder WithDefaults Value
themeColorDecoder def =
    (Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case String.toLower type_ of
                    "rgb" ->
                        Decode.map3 Theme_
                            (Decode.field "red" Decode.int)
                            (Decode.field "green" Decode.int)
                            (Decode.field "blue" Decode.int)
                            |> Decode.map Theme

                    _ ->
                        Decode.fail <| "Unsupported color type"
            )
    )
        |> Json.withDefault def



---------------------------------------------------------------
-- TYPE --
---------------------------------------------------------------


type alias DevApi =
    { toRgb : Value -> ( Int, Int, Int )
    , fromRgb : ( Int, Int, Int ) -> Value
    }


devToRgb : Value -> ( Int, Int, Int )
devToRgb v =
    let
        { r, g, b } =
            toRgba v
    in
    ( r, g, b )


devFromRgb : ( Int, Int, Int ) -> Value
devFromRgb ( r, g, b ) =
    Theme { red = r, green = g, blue = b }


devApi : DevApi
devApi =
    { toRgb = devToRgb
    , fromRgb = devFromRgb
    }
