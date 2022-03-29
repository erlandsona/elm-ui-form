module UI.Form exposing
    ( Model, showErrors
    , init, bool, int, str
    , get, get_, getBool, getBool_, getInt, getList
    , set, setBool, setInt
    , remove
    , Prop
    , Key, key
    , checkbox, underlined
    , Config, field, kind, parser, floatError
    )

{-| Form Builder


# Model

@docs Model, showErrors
@docs init, bool, int, str
@docs get, get_, getBool, getBool_, getInt, getList
@docs set, setBool, setInt
@docs remove
@docs Prop
@docs Key, key


# View

@docs checkbox, underlined


# Builder (for underlined)

@docs Config, field, kind, parser, floatError

-}

import Accessors as A exposing (Relation, name, over)
import Dict exposing (Dict)
import Element.WithContext as E
import Element.WithContext.Events as Event
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Gen.Lens as Lens
import Style.Color as Color
import Style.Size as Size
import UI exposing (Attributes, Element)
import UI.Field as Field
import Util.Bool as BoolUtil
import Util.Fn exposing (flip)
import Util.Maybe as MaybeUtil
import Util.String as StringUtil



{- TODO: Unify Form field definitions under a real form object via a validator
   so that this value is more than just a Phantom.
-}


type Model value
    = Model Internal


type alias Internal =
    { -- NOTE: use Result String String
      -- if parse errors need to be aggregated for display in a toast or something.
      -- data : Maybe value
      form : Dict String String
    , idPrefix : String
    , showErrors : Bool
    }


{-| Form.init Dict.empty

type Msg
= Updated (Form.Model out)

init

update msg model =
Updated newForm ->
{ model | form = newForm }

view : Model -> Element Msg
view model =
input "some-page" TextField.underlined
{ label = "My Field"
}

-}
init :
    { idPrefix : String
    , prefillData : List ( Key value, String )
    }
    -> Model value
init { idPrefix, prefillData } =
    Model <| Internal (Dict.fromList (prefillData |> List.map (Tuple.mapFirst unKey))) idPrefix False


type Key value
    = Key String


unKey : Key value -> String
unKey (Key s) =
    s


type alias Prop value field wrap =
    Relation field field field
    -> Relation value field wrap


key : Prop value field wrap -> Key value
key l =
    Key (name l)


int : Prop value field wrap -> Int -> ( Key value, String )
int l i =
    ( key l, String.fromInt i )


str : Prop value field wrap -> String -> ( Key value, String )
str l s =
    ( key l, s )


bool : Prop value field wrap -> Bool -> ( Key value, String )
bool l b =
    ( key l, BoolUtil.toString b )


{-| Cleanest type but ignores whether or not the field exists.
-}
get : Prop value field wrap -> Model value -> String
get lens =
    get_ lens >> Maybe.withDefault ""


{-| Use this if you need to know if the field has been set previously
-}
get_ : Prop value field wrap -> Model value -> Maybe String
get_ lens (Model { form }) =
    Dict.get (name lens) form


getInt : Prop value field wrap -> Model value -> Maybe Int
getInt lens =
    get_ lens >> Maybe.andThen String.toInt


{-| Cleanest type but ignores whether or not the field exists.
-}
getBool : Prop value field wrap -> Model value -> Bool
getBool lens =
    getBool_ lens >> Maybe.withDefault False


{-| Use this if you need to know if the field has been set previously
-}
getBool_ : Prop value field wrap -> Model value -> Maybe Bool
getBool_ lens =
    get_ lens >> Maybe.andThen BoolUtil.fromString


{-| Form.all Lens.listProp form --> [(name (Lens.listProp << ListUtil.atIdx 0)), "value")]
-}
getList : Prop value field wrap -> Model value -> List ( Int, String )
getList lens (Model { form }) =
    form
        |> Dict.toList
        |> List.filterMap
            (\( k, val ) ->
                k
                    |> String.dropLeft (String.length (name lens))
                    |> String.split "["
                    |> List.filterMap (String.split "]" >> List.filterMap String.toInt >> List.head)
                    |> List.head
                    |> Maybe.map (flip Tuple.pair val)
            )


set : Prop value field wrap -> String -> Model value -> Model value
set lens value (Model i) =
    Model (over Lens.form (Dict.insert (name lens) value) i)


setBool : Prop value field wrap -> Bool -> Model value -> Model value
setBool lens b =
    set lens (BoolUtil.toString b)


setInt : Prop value field wrap -> Int -> Model value -> Model value
setInt lens i =
    set lens (String.fromInt i)


remove : Prop value field wrap -> Model value -> Model value
remove lens (Model i) =
    Model (over Lens.form (Dict.remove (name lens)) i)


showErrors : Model value -> Model value
showErrors (Model i) =
    Model (A.set Lens.showErrors True i)



-- View


checkbox :
    List (E.Attribute ctx Never)
    ->
        { icon : Bool -> E.Element ctx (Model value)
        , label : Input.Label ctx (Model value)
        , key : Prop value field wrap
        }
    -> Model value
    -> E.Element ctx (Model value)
checkbox attrs config (Model f) =
    let
        key_ : String
        key_ =
            name config.key

        id : String
        id =
            f.idPrefix ++ "-" ++ StringUtil.dasherize key_
    in
    Input.checkbox
        (UI.id id
            :: (attrs |> List.map (E.mapAttribute never))
        )
        { onChange = flip (setBool config.key) (Model f)
        , checked = getBool config.key (Model f)
        , icon = config.icon
        , label = config.label
        }


type alias Config value field wrap =
    { description : Field.Description

    -- TODO: Generalize so Form can handle nested Structures
    , key : Prop value field wrap
    , parser : String -> Result String field
    , kind : Field.Config (Model value)
    , floatErrors : Bool
    }


field :
    { description : Field.Description
    , key : Prop value field wrap
    , parser : String -> Result String field
    }
    -> Config value field wrap
field c =
    { description = c.description
    , key = c.key
    , parser = c.parser
    , kind = Field.text
    , floatErrors = False
    }


parser : (String -> Result String field) -> Config value field wrap -> Config value field wrap
parser =
    A.set Lens.parser


kind : Field.Config (Model value) -> Config value field wrap -> Config value field wrap
kind =
    A.set Lens.kind


floatError : Config value field wrap -> Config value field wrap
floatError =
    A.set Lens.floatErrors True


underlined :
    Model value
    -> Attributes Never
    -> Config value field wrap
    -> Element (Model value)
underlined (Model f_) attrs config =
    let
        key_ : String
        key_ =
            name config.key

        id : String
        id =
            f.idPrefix ++ "-" ++ StringUtil.dasherize key_

        validated : Internal -> Internal
        validated =
            BoolUtil.when (Dict.get key_ f_.form == Nothing || f_.showErrors)
                (over Lens.form (Dict.update key_ (MaybeUtil.orElse (Just ""))))

        f : Internal
        f =
            if f_.showErrors then
                f_ |> validated

            else
                f_

        value_ : Maybe String
        value_ =
            Dict.get key_ f.form

        parsed : Maybe (Result String field)
        parsed =
            Maybe.map config.parser value_

        valid : Bool
        valid =
            case parsed of
                Just (Ok _) ->
                    True

                Just (Err _) ->
                    False

                Nothing ->
                    True
    in
    if config.floatErrors then
        Field.underlined
            (UI.id id
                :: Event.onLoseFocus (Model (validated f))
                :: E.below (errors id [ E.moveDown 4 ] parsed)
                :: (attrs |> List.map (E.mapAttribute never))
            )
            config.description
            (config.kind |> Field.isValid valid)
            (flip (set config.key) (Model f))
            (value_ |> Maybe.withDefault "")

    else
        E.column (attrs |> List.map (E.mapAttribute never))
            [ Field.underlined
                [ UI.id id
                , Event.onLoseFocus (Model (validated f))
                ]
                config.description
                (config.kind |> Field.isValid valid)
                (flip (set config.key) (Model f))
                (value_ |> Maybe.withDefault "")
            , errors id [] parsed
            ]


errors : String -> Attributes msg -> Maybe (Result String field) -> Element msg
errors id overrides parsed =
    case parsed of
        Just (Err e) ->
            errorRow (UI.id (id ++ "-err") :: overrides) e

        _ ->
            errorRow overrides
                (Char.fromCode 127
                    -- Blank character to match height of
                    -- potential error
                    |> String.fromChar
                )


errorRow : Attributes msg -> String -> Element msg
errorRow overrides errorMsg =
    E.el
        (Font.color (Color.toUI Color.redValue)
            :: UI.fontSize Size.xsmall
            :: overrides
        )
        (E.text errorMsg)
