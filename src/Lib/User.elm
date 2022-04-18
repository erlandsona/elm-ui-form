module Lib.User exposing (Street(..), User, Zip(..), decoder, encode)

import Json.Decode as D exposing (Decoder)
import Json.Encode as Encode
import Util.Email as Email exposing (Email)
import Util.Maybe as MaybeUtil


type Zip
    = Zip Int


type Street
    = Street String


type alias User =
    { id : Int
    , name : String
    , email : Maybe Email
    , altEmails : List Email
    , address : ( Zip, Street )
    }


decoder : Decoder User
decoder =
    D.map5 User
        (D.field "id" D.int)
        (D.field "name" D.string)
        (D.field "email" (D.nullable Email.decoder))
        (D.field "alt_emails"
            (D.list (D.maybe Email.decoder)
                |> D.map (List.filterMap identity)
            )
        )
        (D.field "address"
            (D.map2 Tuple.pair
                (D.index 0 D.int |> D.map Zip)
                (D.index 1 D.string |> D.map Street)
            )
        )


encode : User -> Encode.Value
encode u =
    Encode.object
        [ ( "id", Encode.int u.id )
        , ( "name", Encode.string u.name )
        , ( "email"
          , MaybeUtil.ifElse u.email
                Email.encode
                Encode.null
          )
        ]
