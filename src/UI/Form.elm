module UI.Form exposing
    ( Model
    , init, bool, int, str
    , remove
    , checkbox, underlined
    , Config, field, kind, parser, floatError
    ,  getList
      , showErrors
        -- , setList
        -- ,  tuple

    )

{-| Form Builder


# Model

@docs Model
@docs init, bool, int, str
@docs remove


# View

@docs checkbox, underlined


# Builder (for underlined)

@docs Config, field, kind, parser, floatError

-}

import Accessors as A exposing (Lens, get, name, over, set)
import Dict exposing (Dict)
import Element.WithContext as E
import Element.WithContext.Events as Event
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Gen.Lens as Lens
import Style.Size as Size
import UI exposing (Attributes, Element)
import UI.Field as Field
import Util.Bool as BoolUtil
import Util.Maybe as MaybeUtil
import Util.String as StringUtil



{- TODO: Unify Form field definitions under a real form object via a validator
   so that this value is more than just a Phantom.
-}


type Model value
    = Model Internal


c_Model : Lens (Model value) transformed Internal built
c_Model =
    let
        getter (Model i) =
            i
    in
    A.makeOneToOne_ "_Model" getter (\fn -> getter >> fn >> Model)


type alias Internal =
    { -- NOTE: use Result String String
      -- if parse errors need to be aggregated for display in a toast or something.
      -- data : Maybe value
      form : Dict String String
    , idPrefix : String
    , showErrors : Bool
    }


formL : Lens (Model value) transformed (Dict String String) built
formL =
    c_Model << Lens.form


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
    }
    -> Model value
init { idPrefix } =
    Model
        { form = Dict.empty
        , idPrefix = idPrefix
        , showErrors = False
        }


str : Lens value transformed field built -> Lens (Model value) transformed (Maybe String) built
str lens =
    formL << A.key (name lens)


int :
    Lens value transformed field built
    -- -> Relation (Maybe Int) reachable a
    -- -> Relation (Model value) reachable a
    -> Lens (Model value) transformed (Maybe Int) built
int lens =
    A.makeOneToOne_ (name lens)
        (get (formL << A.key (name lens))
            >> Maybe.andThen String.toInt
        )
        (\fn ->
            over (formL << A.key (name lens))
                (Maybe.andThen String.toInt
                    >> fn
                    >> Maybe.map String.fromInt
                )
        )


bool :
    Lens value transformed field built
    -- Relation (Maybe Bool) reachable a
    -- -> Relation (Model value) reachable a
    -> Lens (Model value) transformed (Maybe Bool) built
bool lens =
    A.makeOneToOne_ (name lens)
        (get (formL << A.key (name lens))
            >> Maybe.andThen BoolUtil.fromString
        )
        (\fn ->
            over (formL << A.key (name lens))
                (Maybe.andThen BoolUtil.fromString
                    >> fn
                    >> Maybe.map BoolUtil.toString
                )
        )


{-| tuple
set (Form.tuple (Form.int Lens.id, Form.str Lens.name)) (1, "Things") form
-}



-- tuple :
--     Lens value transformed attribute built
--     -> ( Lens (Model value) transformed (Maybe a) built
--        , Lens (Model value) transformed (Maybe b) built
--        )
--     -> Lens (Model value) transformed (Maybe ( a, b )) built
-- -- tuple : Lens structure transformed ( a, b ) built -> ( relA, relB ) -> Relation unsure built transformed -> Relation ( a, b ) built transformed
-- tuple root ( la, lb ) =
--     A.makeOneToOne_ (name root ++ "(" ++ name la ++ "," ++ name lb ++ ")")
--         (\d -> ( get (root << la) d, get (root << lb) d ))
--         --     (( Maybe a, Maybe b ) -> ( Maybe a, Maybe b )) -> structure -> structure
--         (\fn d ->
--             let
--                 a =
--                     get (root << la) d
--                 b =
--                     get (root << lb) d
--                 ( newA, newB ) =
--                     fn ( a, b )
--             in
--             d
--                 |> set (root << la) newA
--                 |> set lb newB
--         )
-- map2 : (a -> b -> value)
--    -> Property (Model value) (Model a) wrapA
--    -> Property (Model value) (Model b) wrapB
--    -> Property (Model super) (Model value) wrapValue
-- map2 fn ma mb =
--     A.makeOneToN_
-- case get ma of
--   Nothing ->
--     Nothing
--   Just a ->
--     case mb of
--       Nothing ->
--         Nothing
--       Just b ->
--         Just (func a b)
-- to : Record -> Dict String String
-- from : Dict String String -> Record
-- andMap : ()
-- list : Prop (Model value) field wrap -> Property value field wrap -> Prop (Model value) (List field) wrap
-- list formL
-- setList :
--     Lens structure (Maybe transformed) (List String) (Maybe build)
--     -> List String
--     -> Model structure
--     -> Model structure
-- setList lens ls f =
--     List.foldr
--         (\( idx, data ) ->
--             let
--                 -- lhs : Lens structure transformed String (Maybe build)
--                 lhs :
--                     Relation String (Maybe build) transformed
--                     -> Relation structure (Maybe build) (Maybe transformed)
--                 lhs =
--                     lens << A.at idx
--             in
--             set (str lhs) data
--         )
--         f
--         (get A.eachIdx ls)


getList : Lens structure transformed attribute build -> Model structure -> List ( Int, String, String )
getList lens (Model { form }) =
    form
        |> Dict.toList
        |> List.filterMap
            (\( k, val ) ->
                k
                    |> String.dropLeft (String.length (name lens))
                    |> String.split "["
                    |> List.filterMap
                        (\s ->
                            case String.split "]" s of
                                hd :: rest ->
                                    Maybe.map
                                        (\id -> ( id, String.concat rest, val ))
                                        (String.toInt hd)

                                _ ->
                                    Nothing
                        )
                    |> List.head
            )


remove : Lens value reachable field wrap -> Model value -> Model value
remove lens =
    A.over formL (Dict.remove (name lens))


showErrors : Model value -> Model value
showErrors =
    A.set (c_Model << Lens.showErrors) True



-- View


checkbox :
    List (E.Attribute ctx Never)
    ->
        { icon : Bool -> E.Element ctx (Model value)
        , label : Input.Label ctx (Model value)
        , key : Lens value reachable field wrap
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
        { onChange =
            \b ->
                set (formL << A.key (name config.key))
                    (BoolUtil.toString b |> Just)
                    (Model f)
        , checked =
            get (formL << A.key (name config.key))
                (Model f)
                |> Maybe.andThen BoolUtil.fromString
                |> Maybe.withDefault False
        , icon = config.icon
        , label = config.label
        }


type alias Config value field wrap =
    { description : Field.Description

    -- TODO: Generalize so Form can handle nested Structures
    , key : Lens value value field wrap
    , parser : String -> Result String field
    , kind : Field.Config (Model value)
    , floatErrors : Bool
    }


field :
    { description : Field.Description
    , key : Lens value value field wrap
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
            (\val -> set (formL << A.key (name config.key)) (Just val) (Model f))
            (value_ |> Maybe.withDefault "")

    else
        E.column (attrs |> List.map (E.mapAttribute never))
            [ Field.underlined
                [ UI.id id
                , Event.onLoseFocus (Model (validated f))
                ]
                config.description
                (config.kind |> Field.isValid valid)
                (\val -> set (formL << A.key (name config.key)) (Just val) (Model f))
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
        ((Font.color |> E.withAttribute (.red << .color))
            :: UI.fontSize Size.xsmall
            :: overrides
        )
        (E.text errorMsg)
