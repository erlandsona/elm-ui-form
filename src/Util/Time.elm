module Util.Time exposing
    ( DateFormat
    , dateTimeFormat
    , defaultDateFormat
    , difference
    , format
    , formatDate
    ,  formatDate_
       -- , isSameDay

    , longDateFormat
    , shortDateFormat
    , shortTimeFormat
    , stringFormatToStrftimeFormats
    , timeAttrFormat
    )

import Strftime
import Time
import Util.Bool as BoolUtil


format : Time.Zone -> Time.Posix -> String
format tz =
    Strftime.format "%-I:%M %p %b %d %Y" tz
        >> BoolUtil.when (Time.utc == tz) (\str -> str ++ " UTC")


{-| Used for tearsheet unusual events.
-}
formatDate : Time.Posix -> String
formatDate =
    formatDate_ Time.utc


formatDate_ : Time.Zone -> Time.Posix -> String
formatDate_ =
    Strftime.format "%Y-%m-%d"


stringFormatToStrftimeFormats : Maybe String -> DateFormat
stringFormatToStrftimeFormats configDateFormat =
    case configDateFormat of
        Just "M/d/yy h:mm a" ->
            { date = "%-m/%-d/%y", time = "%-I:%M %p" }

        Just "dd.MM.yyyy h:mm a" ->
            { date = "%d.%m.%Y", time = "%-I:%M %p" }

        Just "MMM d, yyyy h:mm a" ->
            { date = "%b %-d, %Y", time = "%-I:%M %p" }

        Just "dd/MM/yyyy h:mm a" ->
            { date = "%d/%m/%Y", time = "%-I:%M %p" }

        Just "dd.MM.yy h:mm a" ->
            { date = "%d.%m.%y", time = "%-I:%M %p" }

        Just "MM/dd/yyyy h:mm a" ->
            { date = "%m/%d/%Y", time = "%-I:%M %p" }

        Just "M/d/yyyy h:mm a" ->
            { date = "%-m/%-d/%Y", time = "%-I:%M %p" }

        _ ->
            defaultDateFormat


defaultDateFormat : DateFormat
defaultDateFormat =
    { date = "%-m/%-d/%y", time = "%-I:%M %p" }


type alias DateFormat =
    { date : String
    , time : String
    }


shortDateFormat : Time.Zone -> Time.Posix -> String
shortDateFormat timezone posix =
    Strftime.format "%b %d" timezone posix


longDateFormat : Time.Zone -> Time.Posix -> String
longDateFormat timezone posix =
    --TODO locale
    -- e.g. September 3, 2010
    Strftime.format "%B %-d, %Y" timezone posix


shortTimeFormat : Time.Zone -> Time.Posix -> String
shortTimeFormat timezone posix =
    --TODO locale
    -- e.g. 12:05 PM
    Strftime.format "%-I:%M %p" timezone posix


dateTimeFormat : Time.Zone -> Time.Posix -> String
dateTimeFormat timezone posix =
    --TODO locale
    -- e.g. 2020-05-13 04:17 PM
    Strftime.format "%Y-%m-%d %H:%M %p" timezone posix


timeAttrFormat : Time.Zone -> Time.Posix -> String
timeAttrFormat timezone posix =
    Strftime.format "%Y-%m-%dT%H:%M:%S" timezone posix


difference : Time.Posix -> Time.Posix -> Int
difference t0 t1 =
    abs (Time.posixToMillis t0 - Time.posixToMillis t1)
