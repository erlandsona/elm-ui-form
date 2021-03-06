module Pages.Home_ exposing (Model, Msg, page)

import Accessors exposing (at, eachIdx, fst, get, over, set, snd)
import Element.WithContext as E
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Input as Input
import Gen.Lens as Lens
import Gen.Params.Home_ exposing (Params)
import Lib.User exposing (User)
import Page
import Process
import Request
import Shared
import Task
import Time
import UI.Field as Field exposing (Description(..))
import UI.Form as Form
import UI.Time
import Util.Email as Email
import Util.Fn exposing (flip)
import Util.String as StringUtil
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.element
        { init =
            ( { form = Form.init { idPrefix = "user-form" }
              }
            , Process.sleep 1000
                |> Task.perform (always (GotData remoteUserData))
            )
        , update = update
        , view = view shared.time
        , subscriptions = always Sub.none
        }


type alias Model =
    { form : Form
    }


type alias Form =
    Form.Model
        { data : User
        , changePassword : Bool
        }



-- UPDATE


type Msg
    = FormMsg Form
    | GotData UserData


type alias UserData =
    { id : Int
    , name : String
    , email : String
    , altEmails : List String
    , address : ( Int, String )
    }


remoteUserData : UserData
remoteUserData =
    { id = 1
    , name = "Me"
    , email = "me@gmail.com"
    , altEmails = [ "you@gmail.com" ]
    , address = ( 12345, "1234 5th Ave" )
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
    case msg of
        GotData user ->
            let
                setEmail : ( Int, String ) -> Form -> Form
                setEmail ( idx_, email ) =
                    set (Form.str (Lens.data << Lens.altEmails << at idx_)) (Just email)

                setEmails : Form -> Form
                setEmails form =
                    List.foldr setEmail form (get eachIdx user.altEmails)
            in
            over Lens.form
                (set (Form.int (Lens.data << Lens.id)) (Just user.id)
                    >> set (Form.str (Lens.data << Lens.name)) (Just user.name)
                    >> setEmails
                    >> set (Form.int (Lens.data << Lens.address << fst)) (Just (Tuple.first user.address))
                    >> set (Form.str (Lens.data << Lens.address << snd)) (Just (Tuple.second user.address))
                )
                >> none

        FormMsg f ->
            set Lens.form f >> none


none : m -> ( m, Cmd msg )
none m =
    ( m, Cmd.none )



-- VIEW


view : Time.Posix -> Model -> View Msg
view time { form } =
    { title = "Homepage"
    , body =
        E.column
            [ E.width E.fill
            , E.height E.fill
            , E.padding 25
            , E.spacing 10
            ]
            [ E.link [] { url = "/test1", label = E.text "test" }
            , E.paragraph [ E.spacing 5 ]
                [ E.text "Hello, world! It's "
                , UI.Time.view time
                , E.text "FOR USER_ID:"
                , E.text (get (Form.str (Lens.data << Lens.id)) form |> Maybe.withDefault "???")
                ]
            , E.map FormMsg <|
                E.column
                    [ E.width (E.px 750)
                    , E.padding 15
                    , E.spacing 10
                    , Border.solid
                    , Border.width 1
                    , Border.rounded 5
                    , Border.color |> E.withAttribute (.secondary << .color)
                    , (\grey ->
                        Border.shadow
                            { offset = ( 2, 2 )
                            , size = 5
                            , blur = 5
                            , color = grey
                            }
                      )
                        |> E.withAttribute (.normal << .gray << .color)
                    , Background.color (E.rgb255 245 245 245)
                    ]
                    (E.row [ E.width E.fill, E.spacing 10 ]
                        [ Form.field
                            { description = Label [] "Name"
                            , key = Lens.data << Lens.name
                            , parser = StringUtil.nonEmpty >> Result.fromMaybe "Name can't be empty."
                            }
                            |> Form.underlined form
                                [ E.width (E.shrink |> E.minimum 200)
                                ]
                        , Form.field
                            { description = Label [] "Email"
                            , key = Lens.data << Lens.email
                            , parser = Email.parse >> Result.map Just
                            }
                            |> Form.underlined form
                                [ E.width (E.shrink |> E.minimum 200)
                                ]
                        ]
                        :: Form.checkbox []
                            { icon = Field.checkbox
                            , label = Input.labelLeft [] (E.text "Change Password?")
                            , key = Lens.changePassword
                            }
                            form
                        :: []
                     -- (let
                     --         altEmails =
                     --             Form.getList (Lens.data << Lens.altEmails) form
                     --         nextIdx : Int
                     --         nextIdx =
                     --             negate 1 - List.length altEmails
                     --     in
                     --     Input.button []
                     --         { label = E.text "Add Alternate Email"
                     --         , onPress = Just (set (Form.str (Lens.data << Lens.altEmails << at nextIdx)) (Just "") form)
                     --         }
                     --         :: List.map
                     --             (\( idx, _, _ ) ->
                     --                 E.row [ E.spacing 10 ]
                     --                     [ Form.field
                     --                         { description = Label [] ("Email " ++ String.fromInt idx)
                     --                         , key = Lens.data << Lens.altEmails << at idx
                     --                         , parser = Email.parse
                     --                         }
                     --                         |> Form.underlined form
                     --                             [ E.width (E.shrink |> E.minimum 200)
                     --                             ]
                     --                     , Input.button []
                     --                         { label = E.text "Delete"
                     --                         , onPress = Just (Form.remove (Lens.data << Lens.altEmails << at idx) form)
                     --                         }
                     --                     ]
                     --             )
                     --             altEmails
                     --    )
                    )
            ]
    }
