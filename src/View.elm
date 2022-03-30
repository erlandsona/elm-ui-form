module View exposing (View, map, none, placeholder)

import Element.WithContext as E
import UI exposing (Element)


type alias View msg =
    { title : String
    , body : Element msg
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , body = E.text str
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , body = E.map fn view.body
    }

