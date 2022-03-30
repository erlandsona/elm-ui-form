module UI_ exposing (Attribute, Attributes, Defaults, Element, Option)

{-|

    The idea here is to define some helper types but that ultimately
    the consumer of this will make more concrete versions of these types
    with specific Color types for the properties specified by Defaults.

-}

import Element.WithContext as E
import Element.WithContext.Input as Input
import Time


{-| Make your own

    type alias Context =
        UI_.Defaults
            { colors :
                { primary : Color
                , secondary : Color
                }
            }

-}
type alias Defaults properties =
    { properties
        | tz : Time.Zone
    }


{-| Make your own

    type alias Element msg =
        UI_.Element Context msg

-}
type alias Element ctx msg =
    E.Element (Defaults ctx) msg


{-| Make your own

    type alias Attribute msg =
        UI_.Attribute Context msg

-}
type alias Attribute ctx msg =
    E.Attribute (Defaults ctx) msg


{-| Make your own

    type alias Attributes msg =
        UI_.Attributes Context msg

-}
type alias Attributes ctx msg =
    List (Attribute ctx msg)


{-| Make your own

    type alias Option msg =
        UI_.Option Context msg

-}
type alias Option ctx value msg =
    Input.Option (Defaults ctx) value msg
