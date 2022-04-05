module Pages.Home_ exposing (Model, Msg, page)

import Accessors exposing (get, over, set)
import Element.WithContext as E
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Input as Input
import Gen.Lens as Lens
import Gen.Params.Home_ exposing (Params)
import Lib.User as User exposing (User)
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
import Util.List as ListUtil
import Util.String as StringUtil
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.element
        { init =
            ( { form = Form.init { idPrefix = "user-form" }
              }
            , Process.sleep 1000
                |> Task.perform (always GotData)
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
    | GotData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
    case msg of
        GotData ->
            over Lens.form (set (Form.int (Lens.data << Lens.id)) 1)
                >> none

        FormMsg f ->
            set Lens.form f >> none


none : m -> ( m, Cmd msg )
none m =
    ( m, Cmd.none )



-- VIEW


view : Time.Posix -> Model -> View Msg
view time ({ form } as model) =
    { title = "Homepage"
    , body =
        E.column
            [ E.width E.fill
            , E.height E.fill
            , E.padding 25
            , E.spacing 10
            ]
            [ E.paragraph [ E.spacing 5 ]
                [ E.text "Hello, world! It's "
                , UI.Time.view time
                , E.text "FOR USER_ID:"
                , E.text (String.fromInt (get (Form.int (Lens.data << Lens.id)) form))
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
                        |> E.withAttribute (.gray << .color)
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
                        :: (let
                                altEmails =
                                    Form.getList (Lens.data << Lens.altEmails) form

                                nextIdx : Int
                                nextIdx =
                                    negate 1 - List.length altEmails
                            in
                            Input.button []
                                { label = E.text "Add Alternate Email"
                                , onPress = Just (Form.set (Lens.data << Lens.altEmails << ListUtil.atIdx nextIdx) "" form)
                                }
                                :: List.map
                                    (\( idx, _ ) ->
                                        E.row [ E.spacing 10 ]
                                            [ Form.field
                                                { description = Label [] ("Email " ++ String.fromInt idx)
                                                , key = Lens.data << Lens.altEmails << ListUtil.atIdx idx
                                                , parser = Email.parse
                                                }
                                                |> Form.underlined form
                                                    [ E.width (E.shrink |> E.minimum 200)
                                                    ]
                                            , Input.button []
                                                { label = E.text "Delete"
                                                , onPress = Just (Form.remove (Lens.data << Lens.altEmails << ListUtil.atIdx idx) form)
                                                }
                                            ]
                                    )
                                    altEmails
                           )
                    )
            ]
    }
