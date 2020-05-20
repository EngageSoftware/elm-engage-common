module Engage.UI.LinkButton exposing (custom, divert, standard)

import Engage.CssHelpers
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Importance(..), Size(..))
import Html exposing (Html)


standard : { namespace : Namespace, attributes : List (Html.Attribute msg), text : String } -> Html msg
standard { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = "Button-Standard-Large"
        , attributes = attributes
        , contents = [ Html.text text ]
        }


divert : { namespace : Namespace, attributes : List (Html.Attribute msg), text : String } -> Html msg
divert { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = "Button-Divert-Large"
        , attributes = attributes
        , contents = [ Html.text text ]
        }


custom : { namespace : Namespace, class : String, attributes : List (Html.Attribute msg), contents : List (Html msg) } -> Html msg
custom { namespace, class, attributes, contents } =
    let
        namespacedClass =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    Html.a
        (namespacedClass [ class ] :: attributes)
        contents
