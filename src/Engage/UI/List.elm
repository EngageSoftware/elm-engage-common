module Engage.UI.List exposing (list)

import Engage.Html.Extra as HtmlExtra
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.UI.List.Css exposing (Class(..))
import Html exposing (..)
import Html.CssHelpers


list : { namespace : Namespace } -> List (List (Html msg)) -> Html msg
list { namespace } items =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    if List.isEmpty items then
        HtmlExtra.none

    else
        ul [ class [ List ] ] (items |> List.map (listItem { namespace = namespace }))


listItem : { namespace : Namespace } -> List (Html msg) -> Html msg
listItem { namespace } content =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    li [ class [ ListItem ] ] content
