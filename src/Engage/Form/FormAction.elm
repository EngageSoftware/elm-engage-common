module Engage.Form.FormAction exposing (formAction)

{-| Form.FormAction

@docs formAction

-}

import Engage.CssHelpers
import Engage.Namespace as Namespace exposing (Namespace)
import Html exposing (..)


{-| Get a form action Html
-}
formAction : Namespace -> List (Html msg) -> List (Html msg) -> Html msg
formAction namespace left right =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    div [ class [ "FormAction" ] ]
        [ div [ class [ "FormActionLeft" ] ] left
        , div [ class [ "FormActionRight" ] ] right
        ]
