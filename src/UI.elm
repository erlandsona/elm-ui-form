module UI exposing
    ( Attribute
    , Attributes
    , Context
    , Element
    , Option
    , area
    , fontSize
    , id
    , length
    , px
    , transparent
    )

import Element.WithContext as E exposing (Length)
import Element.WithContext.Font as Font
import Html.Attributes as A
import Style.Size as Size exposing (Size)
import UI_


type alias Context =
    UI_.Defaults
        { color :
            { primary : E.Color
            , secondary : E.Color
            , gray : E.Color
            }
        }


type alias Element msg =
    UI_.Element Context msg


type alias Attribute msg =
    UI_.Attribute Context msg


type alias Attributes msg =
    List (Attribute msg)


type alias Option value msg =
    UI_.Option Context value msg


id : String -> E.Attribute ctx msg
id =
    A.id >> E.htmlAttribute


area : Length -> List (E.Attribute ctx msg)
area len =
    [ E.width len
    , E.height len
    ]


length : Size -> Int
length =
    Size.toFloat >> remToPx


fontSize : Size -> E.Attribute ctx msg
fontSize =
    Size.fontSizeRem >> remToPx >> Font.size


transparent : E.Color
transparent =
    E.rgba 0 0 0 0


remToPx : Float -> Int
remToPx rem =
    round (rem * 16)


px : Size -> Length
px =
    length >> E.px
