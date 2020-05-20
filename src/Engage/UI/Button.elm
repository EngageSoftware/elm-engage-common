module Engage.UI.Button exposing (custom, divert, divertSmall, negative, negativeSmall, primary, primarySmall, standard, standardSmall)

{-| UI.Button

@docs custom, divert, divertSmall, negative, negativeSmall, primary, primarySmall, standard, standardSmall

-}

import Engage.CssHelpers
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Importance(..), Size(..))
import Html exposing (..)
import Html.Attributes exposing (..)


{-| Get a standard Button
-}
standard : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
standard { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Standard Large
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


{-| Get a primary Button
-}
primary : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
primary { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Primary Large
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


{-| Get a divert Button
-}
divert : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
divert { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Divert Large
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


{-| Get a negative Button
-}
negative : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
negative { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Negative Large
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


{-| Get a standard small Button
-}
standardSmall : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
standardSmall { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Standard Small
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


{-| Get a primary small Button
-}
primarySmall : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
primarySmall { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Primary Small
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


{-| Get a divert small Button
-}
divertSmall : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
divertSmall { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Divert Small
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


{-| Get a negative small Button
-}
negativeSmall : { namespace : Namespace, attributes : List (Attribute msg), text : String } -> Html msg
negativeSmall { attributes, text, namespace } =
    custom
        { namespace = namespace
        , class = Button Negative Small
        , attributes = type_ "button" :: attributes
        , contents = [ Html.text text ]
        }


{-| Get a custom Button
-}
custom : { namespace : Namespace, class : class, attributes : List (Attribute msg), contents : List (Html msg) } -> Html msg
custom { namespace, class, attributes, contents } =
    let
        namespaced =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    button
        (namespaced.class [ toString BaseButton, toString class ] :: attributes)
        contents
