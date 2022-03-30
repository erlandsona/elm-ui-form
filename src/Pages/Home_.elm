module Pages.Home_ exposing (Model, Msg, page)

import Accessors exposing (set)
import Element.WithContext as E
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Input as Input
import Gen.Lens as Lens
import Gen.Params.Home_ exposing (Params)
import Lib.User as User exposing (User)
import Page
import Request
import Shared
import Time
import UI.Field as Field exposing (Description(..))
import UI.Form as Form
import UI.Time
import Util.Email as Email
import Util.String as StringUtil
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.element
        { init =
            ( { form = Form.init { idPrefix = "user-form", prefillData = [] }
              }
            , Cmd.none
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
    case msg of
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
            [ E.paragraph []
                [ E.text "Hello, world! It's "
                , UI.Time.view time
                ]
            , E.map FormMsg <|
                E.column
                    [ E.width (E.px 750)
                    , E.height (E.px 750)
                    , E.padding 15
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

                    -- , Form.field
                    --     { description = Label [] "Change Password?"
                    --     , key = Lens.changePassword
                    --     , parser = Ok
                    --     }
                    --     |>
                    , Form.checkbox []
                        { icon = Field.checkbox
                        , label = Input.labelLeft [] (E.text "Change Password?")
                        , key = Lens.changePassword
                        }
                        form
                    ]
            ]
    }
