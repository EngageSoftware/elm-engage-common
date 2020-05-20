module Engage.Pattern.CardCollector exposing
    ( Attribute
    , addButton, none, title, view
    )

{-| Pattern.CardCollector

@docs Attribute

@docs addButton, none, title, view

-}

import Engage.CssHelpers
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.UI.Button as Button
import Html exposing (..)
import Html.Events exposing (onClick)
import String


type alias InternalAttribute msg =
    { titleText : Maybe String
    , addButtonText : Maybe ( String, msg )
    }


emptyAttribute : InternalAttribute msg
emptyAttribute =
    { titleText = Nothing
    , addButtonText = Nothing
    }


{-| The Attribute type
-}
type alias Attribute msg =
    InternalAttribute msg -> InternalAttribute msg


processAttribute : config -> List (config -> config) -> config
processAttribute initialAttribute configs =
    List.foldl (\f config -> f config) initialAttribute configs


{-| Get the none attribute
-}
none : Attribute msg
none =
    identity


{-| Get the title attribute
-}
title : String -> Attribute msg
title text =
    \attribute -> { attribute | titleText = Just text }


{-| Get the addButton attribute
-}
addButton : String -> msg -> Attribute msg
addButton text msg =
    \attribute -> { attribute | addButtonText = Just ( text, msg ) }


{-| Get the view
-}
view : Namespace -> List (Attribute msg) -> List (Html msg) -> Html msg
view namespace attributes cards =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        attribute =
            processAttribute emptyAttribute attributes
    in
    div [ class [ CardCollector ] ]
        [ titleView namespace attribute
        , div [ class [ CardCollectorCards ] ] cards
        , actionView namespace attribute
        ]


titleView : Namespace -> InternalAttribute msg -> Html msg
titleView namespace attribute =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        viewHelper titleText =
            if String.isEmpty titleText then
                text ""

            else
                h2 [ class [ CardCollectorTitle ] ] [ text titleText ]
    in
    attribute.titleText
        |> Maybe.map viewHelper
        |> Maybe.withDefault (Html.text "")


actionView : Namespace -> InternalAttribute msg -> Html msg
actionView namespace attribute =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    div [ class [ CardCollectorAction ] ]
        [ attribute.addButtonText
            |> Maybe.map
                (\( addButtonText, msg ) ->
                    Button.primary
                        { namespace = namespace
                        , attributes = [ onClick msg ]
                        , text = addButtonText
                        }
                )
            |> Maybe.withDefault (Html.text "")
        ]
