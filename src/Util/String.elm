module Util.String exposing (dasherize)

import Regex exposing (Regex)


{-| Return a string joined by dashes after separating it by its uppercase characters.
Any sequence of spaces or underscores will also be converted to a single dash.
The final string will be lowercased.
dasherize "SomeClassName" == "-some-class-name"
dasherize "some\_class\_name" = "some-class-name"
dasherize "someClass name" = "some-class-name"
-}
dasherize : String -> String
dasherize string =
    string
        |> String.trim
        |> Regex.replace (regexFromString "([A-Z])") (.match >> String.append "-")
        |> Regex.replace (regexFromString "[_-\\s]+") (always "-")
        |> String.toLower


regexFromString : String -> Regex
regexFromString =
    Regex.fromString >> Maybe.withDefault Regex.never
