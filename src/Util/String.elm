module Util.String exposing
    ( caseInsensitiveContains
    , containsIgnoreCase
    , dasherize
    , ellipsis
    , humanize
    , nonBreakingSpace
    , nonEmpty
    , pluralize
    , present
    , smartCaseContains
    , stripTickerSuffix
    , toTitleCase
    , underscored
    )

import Regex exposing (Regex)


caseInsensitiveContains : String -> String -> Bool
caseInsensitiveContains sub str =
    String.contains (String.toLower sub) (String.toLower str)


containsIgnoreCase : String -> String -> Bool
containsIgnoreCase =
    caseInsensitiveContains


smartCaseContains : String -> String -> Bool
smartCaseContains sub str =
    let
        isCaseSensitive : Bool
        isCaseSensitive =
            String.any Char.isUpper sub
    in
    if isCaseSensitive then
        String.contains sub str

    else
        caseInsensitiveContains sub str


nonBreakingSpace : String
nonBreakingSpace =
    "\u{00A0}"


humanize : String -> String
humanize string =
    string
        |> Regex.replace (regexFromString "[A-Z]+") (.match >> String.append "-")
        |> Regex.replace (regexFromString "_id$|[-_\\s]+") (always " ")
        |> String.trim
        |> String.toLower
        |> toSentenceCase


nonEmpty : String -> Maybe String
nonEmpty str =
    if String.isEmpty str then
        Nothing

    else
        Just str


pluralize : String -> String -> Int -> String
pluralize singular plural n =
    case n of
        1 ->
            singular

        _ ->
            plural


{-| I swiped this from elm-community/string-extra -- Chad Aug 26 2020

Return a string joined by underscores after separating it by its uppercase characters.
Any sequence of spaces or dashes will also be converted to a single underscore.
The final string will be lowercased.

    underscored "SomeClassName" == "some\_class\_name"
    underscored "some-class-name" == "some\_class\_name"
    underscored "SomeClass name" == "some\_class\_name

-}
underscored : String -> String
underscored string =
    string
        |> String.trim
        |> Regex.replace (regexFromString "([a-z\\d])([A-Z]+)") (.submatches >> List.filterMap identity >> String.join "_")
        |> Regex.replace (regexFromString "[_-\\s]+") (always "_")
        |> String.toLower


ellipsis : Int -> String -> String
ellipsis howLong string =
    if String.length string <= howLong then
        string

    else
        String.left (howLong - 1) string ++ "…"


present : String -> Bool
present =
    not << String.isEmpty


toTitleCase : String -> String
toTitleCase ws =
    let
        uppercaseMatch : String -> String
        uppercaseMatch =
            Regex.replace (regexFromString "\\w+") (.match >> toSentenceCase)
    in
    ws
        |> Regex.replace
            (regexFromString "^([a-z])|\\s+([a-z])")
            (.match >> uppercaseMatch)


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


{-| Change the case of the first letter of a string to either uppercase or
lowercase, depending of the value of `wantedCase`. This is an internal
function for use in `toSentenceCase` and `decapitalize`.
-}
changeCase : (Char -> Char) -> String -> String
changeCase mutator word =
    String.uncons word
        |> Maybe.map (\( head, tail ) -> String.cons (mutator head) tail)
        |> Maybe.withDefault ""


{-| Capitalize the first letter of a string.
toSentenceCase "this is a phrase" == "This is a phrase"
toSentenceCase "hello, world" == "Hello, world"
-}
toSentenceCase : String -> String
toSentenceCase word =
    changeCase Char.toUpper word


regexFromString : String -> Regex
regexFromString =
    Regex.fromString >> Maybe.withDefault Regex.never


{-| Strip the exchange suffix from a ticker.
stripTickerSuffix "AAPL US" == "AAPL"
stripTickerSuffix "AAPL.N" == "AAPL"
stripTickerSuffix "AAPL" == "AAPL"
-}
stripTickerSuffix : String -> String
stripTickerSuffix s =
    let
        stripAfterDelim : Char -> String -> String
        stripAfterDelim d x =
            String.split (String.fromChar d) x
                |> List.take 1
                |> String.concat
    in
    s
        |> stripAfterDelim '.'
        |> stripAfterDelim ' '
