module Util.Result exposing (toDecoder, toDecoder_)

import Json.Decode as Decode exposing (Decoder)


toDecoder : Result Decode.Error a -> Decoder a
toDecoder =
    Result.mapError Decode.errorToString
        >> toDecoder_


toDecoder_ : Result String a -> Decoder a
toDecoder_ r =
    case r of
        Ok a ->
            Decode.succeed a

        Err str ->
            Decode.fail str
