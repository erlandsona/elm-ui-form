module UI.Field exposing
    ( Description(..), outlined, underlined
    , Config, type_, text, search, multiline
    , isValid, onKeydown
    , checkbox, checkbox_, circle, option
    , checkmark, email, newPassword
    )

{-| Module for creating a styled text input.


# Renderers

@docs Description, outlined, underlined


# Config

@docs Config, type_, text, search, multiline


# Config modifiers

@docs Label, isValid, onKeydown


# Misc / TODO

@docs checkbox, checkbox_, circle, option

-}

import Element.WithContext as E
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Html.Attributes as Attr
import Html.Events as HEv
import Json.Decode as Decode
import Keyboard.Event exposing (KeyboardEvent, considerKeyboardEvent)
import Style.Color as Color
import Style.Size as Size
import UI exposing (Attributes, Element)
import UI.TLDR exposing (edges)
import Util.Fn exposing (uncurry)
import Util.Maybe as MaybeUtil


type Description
    = Label (Attributes Never) String
    | Both { label : ( Attributes Never, String ), placeholder : ( Attributes Never, String ) }
    | Placeholder (Attributes Never) String


type Config msg
    = Config (Internal msg)


type alias Internal msg =
    { content : Content
    , onKeydown : Maybe (KeyboardEvent -> Maybe ( { preventDefault : Bool }, msg ))
    , valid : Bool
    }


type Content
    = Text
      -- | TextWithoutAutocomplete
      -- | Username
    | NewPassword { show : Bool }
      -- | Password { show : Bool }
    | Email
      -- | SpellChecked
    | Search
    | Multiline { spellcheck : Bool }


config : Content -> Config msg
config content =
    Config
        { content = content
        , onKeydown = Nothing
        , valid = True
        }


type_ : Config msg -> (Config msg -> Config msg) -> Config msg
type_ conf fn =
    fn conf


text : Config msg
text =
    config Text


search : Config msg
search =
    config Search


email : Config msg
email =
    config Email


newPassword : { show : Bool } -> Config msg
newPassword =
    config << NewPassword


multiline : { spellcheck : Bool } -> Config msg
multiline =
    config << Multiline


isValid : Bool -> Config msg -> Config msg
isValid bool (Config c) =
    Config { c | valid = bool }


onKeydown : (KeyboardEvent -> Maybe ( { preventDefault : Bool }, msg )) -> Config msg -> Config msg
onKeydown keyMsg (Config c) =
    Config { c | onKeydown = Just keyMsg }


outlined :
    Attributes msg
    -> Description
    -> Config msg
    -> (String -> msg)
    -> String
    -> Element msg
outlined overrides label ((Config c) as conf) =
    base
        (Border.rounded (UI.length Size.medium)
            :: Border.color (borderColor c.valid Color.grayValue4)
            :: overrides
        )
        label
        conf


borderColor : Bool -> Color.Value -> E.Color
borderColor valid clr =
    Color.toUI <|
        if valid then
            clr

        else
            Color.redValue


underlined :
    Attributes msg
    -> Description
    -> Config msg
    -> (String -> msg)
    -> String
    -> Element msg
underlined overrides label ((Config c) as conf) =
    base
        (Border.widthEach { edges | bottom = UI.length Size.xxsmall }
            :: Border.rounded 0
            :: Background.color UI.transparent
            :: Border.color (borderColor c.valid Color.grayValue1)
            :: E.focused
                [ Border.color |> E.withDecoration .primaryColor
                , Font.color (Color.toUI Color.blackValue)
                ]
            :: E.width E.fill
            :: overrides
        )
        label
        conf



--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------


base :
    Attributes msg
    -> Description
    -> Config msg
    -> (String -> msg)
    -> String
    -> Element msg
base overrides label_ (Config c) onChange value =
    let
        attrs : Attributes msg
        attrs =
            E.paddingXY 0 (UI.length Size.medium)
                :: overrides
                |> MaybeUtil.fold
                    (\msgFn ->
                        HEv.preventDefaultOn "keydown"
                            (considerKeyboardEvent msgFn
                                |> Decode.map
                                    (\( { preventDefault }, m ) ->
                                        ( m, preventDefault )
                                    )
                            )
                            |> E.htmlAttribute
                            |> (::)
                    )
                    c.onKeydown

        ( label, placeholder ) =
            case label_ of
                Both desc ->
                    ( uncurry Input.labelAbove
                        (desc.label
                            |> Tuple.mapBoth
                                (\attrs_ ->
                                    UI.fontSize Size.xsmall
                                        :: (attrs_ |> List.map (E.mapAttribute never))
                                )
                                E.text
                        )
                    , Just
                        (uncurry Input.placeholder
                            (desc.placeholder
                                |> Tuple.mapBoth
                                    (List.map (E.mapAttribute never))
                                    E.text
                            )
                        )
                    )

                Label attrs_ lbl ->
                    ( Input.labelAbove
                        (UI.fontSize Size.xsmall
                            :: (attrs_ |> List.map (E.mapAttribute never))
                        )
                        (E.text lbl)
                    , Nothing
                    )

                Placeholder attrs_ ph ->
                    ( Input.labelHidden ph
                    , Just
                        (Input.placeholder
                            (attrs_
                                |> List.map (E.mapAttribute never)
                            )
                            (E.text ph)
                        )
                    )
    in
    case c.content of
        Multiline { spellcheck } ->
            Input.multiline attrs
                { onChange = onChange
                , text = value
                , placeholder = placeholder
                , label = label
                , spellcheck = spellcheck
                }

        Text ->
            Input.text attrs
                { onChange = onChange
                , text = value
                , placeholder = placeholder
                , label = label
                }

        Search ->
            -- TODO: disable autocomplete - Input.search???
            Input.search attrs
                { onChange = onChange
                , text = value
                , placeholder = placeholder
                , label = label
                }

        Email ->
            Input.email attrs
                { onChange = onChange
                , text = value
                , placeholder = placeholder
                , label = label
                }

        NewPassword { show } ->
            Input.newPassword attrs
                { onChange = onChange
                , text = value
                , placeholder = placeholder
                , label = label
                , show = show
                }


checkbox : Bool -> Element msg
checkbox checked =
    E.with .primaryColor
        (checkbox_
            [ E.htmlAttribute (Attr.class "focusable") ]
            checked
        )


checkbox_ : Attributes msg -> Bool -> E.Color -> Element msg
checkbox_ overrides checked pc =
    checkbox__
        (Background.color
            (if checked then
                pc

             else
                Color.toUI Color.whiteValue
            )
            :: Border.rounded 3
            :: Border.color
                (if checked then
                    pc

                 else
                    E.rgb255 211 211 211
                )
            :: Border.shadow
                { offset = ( 0, 0 )
                , blur = 1
                , size = 1
                , color =
                    let
                        alpha : Float
                        alpha =
                            if checked then
                                0

                            else
                                1
                    in
                    E.rgba 238 238 238 alpha
                }
            :: Border.width
                (if checked then
                    0

                 else
                    1
                )
            :: overrides
        )
        checked
        pc


checkbox__ : Attributes msg -> Bool -> E.Color -> Element msg
checkbox__ overrides checked pc =
    E.el
        (E.width (E.px 14)
            :: E.height (E.px 14)
            :: Font.color (Color.contrastingFontColor_ pc)
            :: E.centerY
            :: Font.size 9
            :: Font.center
            :: E.inFront (checkmark checked (Color.contrastingFontColor_ pc))
            :: overrides
        )
        E.none


checkmark : Bool -> E.Color -> Element msg
checkmark checked color =
    E.el
        [ Border.color color
        , E.height (E.px 6)
        , E.width (E.px 9)
        , E.rotate (degrees -45)
        , E.centerX
        , E.centerY
        , E.moveUp 1
        , E.transparent (not checked)
        , Border.widthEach
            { top = 0
            , left = 2
            , bottom = 2
            , right = 0
            }
        ]
        E.none


option : value -> Element msg -> UI.Option value msg
option value label =
    Input.optionWith value
        (\state ->
            E.row [ E.spacing (UI.length Size.medium) ]
                [ circle (state == Input.Selected)
                , label
                ]
        )


circle : Bool -> Element msg
circle on =
    E.el
        (Border.rounded 100
            :: Border.width 1
            :: E.inFront
                (if on then
                    E.el
                        (Border.rounded 100
                            :: E.centerY
                            :: E.centerX
                            :: Background.color (Color.toUI Color.blackValue)
                            :: UI.area (UI.px Size.medium)
                        )
                        E.none

                 else
                    E.none
                )
            :: UI.area (UI.px Size.large)
        )
        E.none
