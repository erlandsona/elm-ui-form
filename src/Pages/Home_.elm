module Pages.Home_ exposing (Model, Msg, page)

import Element.WithContext as E
import Gen.Params.Home_ exposing (Params)
import Page
import Request
import Shared
import Time
import UI.Form as Form
import UI.Time
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.element
        { init = ( (), Cmd.none )
        , update = update
        , view = view shared.time
        , subscriptions = always Sub.none
        }


type alias Model =
    ()



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Cmd.none )



-- VIEW


view : Time.Posix -> Model -> View msg
view time model =
    { title = "Homepage"
    , body =
        E.column []
            [ E.paragraph []
                [ E.text "Hello, world! It's "
                , UI.Time.view time
                ]

            -- , Form.
            ]
    }
