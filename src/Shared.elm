module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    )

import Accessors exposing (set)
import Dict
import Gen.Lens as Lens
import Request exposing (Request)
import Time
import TimeZone
import Util.Fn exposing (flip)


type alias Flags =
    { initialTime : Int
    , tzName : String
    }


type alias Model =
    { time : Time.Posix
    , tz : Time.Zone
    }


type Msg
    = Tick Time.Posix


init : Request -> Flags -> ( Model, Cmd Msg )
init _ f =
    ( { time = Time.millisToPosix f.initialTime
      , tz = (Dict.get f.tzName TimeZone.zones |> Maybe.withDefault (\() -> Time.utc)) ()
      }
    , Cmd.none
    )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg =
    case msg of
        Tick time ->
            set Lens.time time
                >> flip Tuple.pair Cmd.none


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Time.every 1000 Tick
