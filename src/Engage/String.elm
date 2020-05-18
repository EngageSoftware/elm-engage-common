module Engage.String exposing
    ( append
    , comma
    , space
    , toSafeCssClassName
    , toSafeId
    )

{-| String helpers

@docs append

@docs comma

@docs space

@docs toSafeCssClassName

@docs toSafeId

-}

import Char
import Regex exposing (Regex)
import String

{-| Append two strings with a delimeter
-}
append : String -> String -> String -> String
append delimiter second first =
    if first |> String.trim |> String.isEmpty then
        second

    else if second |> String.trim |> String.isEmpty then
        first

    else
        first ++ delimiter ++ second


{-| Add a space between two strings
-}
space : String -> String -> String
space =
    append " "


{-| Add a comma between two strings
-}
comma : String -> String -> String
comma =
    append ", "


cssClassRegex : Regex
cssClassRegex =
    Regex.regex "[^a-z0-9]"


idRegex : Regex
idRegex =
    Regex.regex "[^a-zA-Z0-9]"


{-| Convert to a safe CSS class name
-}
toSafeCssClassName : String -> String
toSafeCssClassName text =
    let
        replace match =
            let
                char =
                    match.match
                        |> String.toList
                        |> List.head
                        |> Maybe.withDefault '-'

                code =
                    Char.toCode char
            in
            if code == 32 then
                "-"

            else if code >= 65 && code <= 90 then
                "_" ++ (char |> String.fromChar |> String.toLower)

            else
                ""
    in
    text
        |> Regex.replace Regex.All cssClassRegex replace


{-| Convert to a safe ID
-}
toSafeId : String -> String
toSafeId text =
    let
        replace match =
            ""
    in
    text
        |> Regex.replace Regex.All idRegex replace
