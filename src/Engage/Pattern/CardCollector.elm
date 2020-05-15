module Engage.Pattern.CardCollector exposing
    ( Attribute
    , addButton
    , none
    , title
    , view
    )

import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Pattern.CardCollector.Css exposing (Class(CardCollector, CardCollectorAction, CardCollectorCards, CardCollectorTitle))
import Engage.UI.Button as Button
import Html exposing (..)
import Html.CssHelpers
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


type alias Attribute msg =
    InternalAttribute msg -> InternalAttribute msg


processAttribute : config -> List (config -> config) -> config
processAttribute initialAttribute configs =
    List.foldl (\f config -> f config) initialAttribute configs


none : Attribute msg
none =
    identity


title : String -> Attribute msg
title text =
    \attribute -> { attribute | titleText = Just text }


addButton : String -> msg -> Attribute msg
addButton text msg =
    \attribute -> { attribute | addButtonText = Just ( text, msg ) }


view : Namespace -> List (Attribute msg) -> List (Html msg) -> Html msg
view namespace attributes cards =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

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
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

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
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
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
