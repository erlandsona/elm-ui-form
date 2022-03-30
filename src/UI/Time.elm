module UI.Time exposing (view)

import Element.WithContext as E
import Time
import UI exposing (Element)
import Util.Time


view : Time.Posix -> Element msg
view time =
    (\tz ->
        Util.Time.timeAttrFormat tz time
            |> E.text
    )
        |> E.with .tz
