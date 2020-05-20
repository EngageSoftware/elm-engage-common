module Engage.CssHelpers exposing (withNamespace)

import Html exposing (Attribute)
import Html.Attributes


withNamespace : String -> (List String -> Attribute msg)
withNamespace name =
    namespacedClass name


namespacedClass : String -> List String -> Attribute msg
namespacedClass name list =
    list
        |> List.map (\_ -> name)
        |> String.join " "
        |> Html.Attributes.class
