module Engage.String exposing
    ( append
    , comma
    , space
    , toSafeCssClassName
    , toSafeId
    )

import Char
import Regex exposing (Regex)
import String


append : String -> String -> String -> String
append delimiter second first =
    if first |> String.trim |> String.isEmpty then
        second

    else if second |> String.trim |> String.isEmpty then
        first

    else
        first ++ delimiter ++ second


space : String -> String -> String
space =
    append " "


comma : String -> String -> String
comma =
    append ", "


cssClassRegex : Regex
cssClassRegex =
    Regex.regex "[^a-z0-9]"


idRegex : Regex
idRegex =
    Regex.regex "[^a-zA-Z0-9]"


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


toSafeId : String -> String
toSafeId text =
    let
        replace match =
            ""
    in
    text
        |> Regex.replace Regex.All idRegex replace
