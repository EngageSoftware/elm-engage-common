module Engage.Svg.CssHelpers exposing (withNamespace)

{-| Svg.CssHelpers

@docs withNamespace

-}

import Svg exposing (Attribute)
import Svg.Attributes as Attr


{-| Get a class function that appends a namespace
-}
withNamespace : String -> (List String -> Attribute msg)
withNamespace name =
    namespacedClass name


namespacedClass : String -> List String -> Attribute msg
namespacedClass name list =
    list
        |> List.map (\class -> name ++ class)
        |> String.join " "
        |> Attr.class
