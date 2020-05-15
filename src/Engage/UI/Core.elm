module Engage.UI.Core exposing (core)

import Css
import Engage.Html.Extra as HtmlExtra
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.CssHelpers


{ id, class, classList } =
    Html.CssHelpers.withNamespace "engageUIDemo"


core : List Css.Stylesheet -> List (Html msg) -> Html msg
core stylesheets children =
    let
        { css } =
            Css.compile stylesheets
    in
    div [ style [ ( "margin", "1em 5em" ) ] ]
        (node "style" [] [ text css ] :: (HtmlExtra.stylesheet "/styles/module.css" :: children))
