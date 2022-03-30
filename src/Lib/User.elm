module Lib.User exposing (User, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Util.Email as Email exposing (Email)
import Util.Maybe as MaybeUtil


type alias User =
    { id : Int
    , name : String
    , email : Maybe Email
    , altEmails : List Email
    }


decoder : Decoder User
decoder =
    Decode.map4 User
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "email" (Decode.nullable Email.decoder))
        (Decode.field "alt_emails"
            (Decode.list (Decode.maybe Email.decoder)
                |> Decode.map (List.filterMap identity)
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
