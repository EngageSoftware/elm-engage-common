module Engage.UI.Core exposing (core)

import Css
import DEPRECATED.Css.File
import Engage.CssHelpers
import Engage.Html.Extra as HtmlExtra
import Html exposing (..)
import Html.Attributes exposing (style)


core : List DEPRECATED.Css.File.Stylesheet -> List (Html msg) -> Html msg
core stylesheets children =
    let
        { css } =
            Css.compile stylesheets
    in
    div [ style [ ( "margin", "1em 5em" ) ] ]
        (node "style" [] [ text css ] :: (HtmlExtra.stylesheet "/styles/module.css" :: children))
