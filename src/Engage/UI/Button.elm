module Engage.UI.Button exposing
    ( custom
    , divert
    , divertSmall
    , negative
    , negativeSmall
    , primary
    , primarySmall
    , standard
    , standardSmall
    )

import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Importance(..), Size(..))
import Engage.UI.Button.Css exposing (Class(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.CssHelpers


standard : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
standard { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Standard Large
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


primary : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
primary { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Primary Large
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


divert : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
divert { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Divert Large
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


negative : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
negative { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Negative Large
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


standardSmall : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
standardSmall { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Standard Small
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


primarySmall : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
primarySmall { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Primary Small
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


divertSmall : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
divertSmall { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Divert Small
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


negativeSmall : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
negativeSmall { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Negative Small
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


custom : { namespace : Namespace, class : class, attributes : List (Attribute msg), contents : List (Html msg) } -> Html msg
custom { namespace, class, attributes, contents } =
    let
        namespaced =
            Html.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    button
        (namespaced.class [ toString BaseButton, toString class ] :: attributes)
        contents
