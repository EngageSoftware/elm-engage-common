module Engage.UI.Link exposing (custom, standard)

import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Importance(..))
import Engage.UI.Link.Css exposing (Class(..))
import Html exposing (Html)
import Html.CssHelpers


standard : { namespace : Namespace, attributes : List (Html.Attribute msg), text : String } -> Html msg
standard { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Link Standard
        , attributes = attributes
        , contents = [ Html.text text ]
        }


custom : { namespace : Namespace, class : Class, attributes : List (Html.Attribute msg), contents : List (Html msg) } -> Html msg
custom { namespace, class, attributes, contents } =
    let
        namespaced =
            Html.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    Html.a
        (namespaced.class [ class ] :: attributes)
        contents
