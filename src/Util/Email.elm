module Util.Email exposing
    ( Email, isValid, parse, toString
    , decoder, encode
    )

{-| Email parser and validation library.

@docs Email, isValid, parse, toString

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser, Problem(..), andThen, chompUntil, chompWhile, end, getChompedString, problem, run, succeed, symbol)
import Util.Decode as DecodeUtil


{-| A model for representing an email. This is exposed, but you'll probably only use it is using parseEmailAddress
-}
type Email
    = Email Internal


type alias Internal =
    { local : String
    , domain : String
    }


{-| Converts the Email model to a string.
-}
toString : Email -> String
toString (Email { local, domain }) =
    local ++ "@" ++ domain


{-| Given a string, parses into an Email model.

        parse "hello@world.com" == Ok { local = "hello", domain = "world.com" }
        parse "^^^^" == Err [ dead ends ]

-}
parse : String -> Result String Email
parse =
    run emailParser
        >> Result.mapError
            (List.map
                (\deadEnd ->
                    case deadEnd.problem of
                        Problem str ->
                            str

                        _ ->
                            "Please enter a valid email"
                )
                >> String.join ", "
            )


decoder : Decoder Email
decoder =
    Decode.andThen
        (parse >> DecodeUtil.fromResult)
        Decode.string


{-| Given a string, this returns true if the email is compatible with the spec.

        isValid "hello@world.com" == True
        isValid "^^^^" == False

-}
isValid : String -> Bool
isValid s =
    run emailParser s
        |> Result.map (always True)
        |> Result.withDefault False


checkLocal : String -> Parser String
checkLocal str =
    let
        isLocalChar : Char -> Bool
        isLocalChar c =
            Char.isAlphaNum c
                || (c == '!')
                || (c == '#')
                || (c == '$')
                || (c == '%')
                || (c == '&')
                || (c == '*')
                || (c == '_')
                || (c == '-')
                || (c == '~')
                || (c == '|')
                || (c == '+')
                || (c == '=')
                || (c == '`')
                || (c == '{')
                || (c == '}')
                || (c == '.')
    in
    if String.isEmpty str then
        problem "local part is empty"

    else if String.startsWith "." str then
        problem "local part cannot start with ."

    else if String.endsWith "." str then
        problem "local part cannot end with ."

    else if String.contains ".." str then
        problem "local part cannot contain .."

    else if String.foldl (\c acc -> acc && isLocalChar c) True str then
        succeed str

    else
        problem "local part contains invalid characters"


localPart : Parser String
localPart =
    chompUntil "@"
        |> getChompedString
        |> andThen checkLocal


domainPart : Parser String
domainPart =
    let
        checkLen : String -> Parser String
        checkLen s =
            if String.isEmpty s then
                problem "domain is empty"

            else if not <| String.contains "." s then
                problem "domain is not valid"

            else
                succeed s
    in
    chompWhile
        (\c -> Char.isAlphaNum c || c == '-' || c == '.')
        |> getChompedString
        |> andThen checkLen


emailParser : Parser Email
emailParser =
    succeed (\local domain -> Email (Internal local domain))
        |= localPart
        |. symbol "@"
        |= domainPart
        |. end


encode : Email -> Encode.Value
encode =
    Encode.string << toString
