module Pages.Test1 exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element.WithContext as E
import Gen.Params.Test1 exposing (Params)
import Page
import Process
import Request
import Shared
import Task
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ _ =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { data : String }


init : ( Model, Cmd Msg )
init =
    ( { data = "Init" }
    , Process.sleep 1500
        |> Task.perform (always GotData)
    )



-- UPDATE


type Msg
    = GotData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotData ->
            ( { data = "Home" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Test1"
    , body =
        E.link []
            { url = "/"
            , label =
                E.text model.data
            }
    }
