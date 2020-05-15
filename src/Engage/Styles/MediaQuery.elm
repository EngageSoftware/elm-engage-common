module Engage.Styles.MediaQuery exposing (BreakPoint(..), atMedia)

import Css exposing (..)


type BreakPoint
    = XSmall
    | Small
    | Medium
    | Large
    | XLarge
    | XXLarge


atMedia : BreakPoint -> List Snippet -> Snippet
atMedia breakPoint =
    breakPoint
        |> toString
        |> mediaQuery


toString : BreakPoint -> String
toString breakPoint =
    case breakPoint of
        XSmall ->
            "(min-width: 400px)"

        Small ->
            "(min-width: 500px)"

        Medium ->
            "(min-width: 700px)"

        Large ->
            "(min-width: 900px)"

        XLarge ->
            "(min-width: 1300px)"

        XXLarge ->
            "(min-width: 1700px)"
