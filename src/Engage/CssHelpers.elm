module Engage.CssHelpers exposing (withNamespace)

import Html exposing (Attribute)


withNamespace : String -> (List String -> Attribute msg)
withNamespace name =
    namespacedClass name


namespacedClass : String -> List String -> Attribute msg
namespacedClass name list =
    list
        |> List.map name
        |> String.join " "
        |> Attr.class
