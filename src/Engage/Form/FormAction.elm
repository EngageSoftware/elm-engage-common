module Engage.Form.FormAction exposing (formAction)

import Engage.Form.FormAction.Css exposing (Class(..))
import Engage.Namespace as Namespace exposing (Namespace)
import Html exposing (..)
import Html.CssHelpers


formAction : Namespace -> List (Html msg) -> List (Html msg) -> Html msg
formAction namespace left right =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    div [ class [ FormAction ] ]
        [ div [ class [ FormActionLeft ] ] left
        , div [ class [ FormActionRight ] ] right
        ]
