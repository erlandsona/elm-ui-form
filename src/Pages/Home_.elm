module Pages.Home_ exposing (view)

import Html
import UI.Form as Form
import View exposing (View)


view : View msg
view =
    { title = "Homepage"
    , body = [ Html.text "Hello, world!" ]
    }
