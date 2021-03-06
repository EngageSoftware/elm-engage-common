module Engage.UI.Card exposing (attributes, card, successCard, edit, none, subtitle, title)

{-| UI.Card

@docs attributes, card, successCard, edit, none, subtitle, title

-}

import Engage.CssHelpers
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.UI.Button as Button
import Html exposing (..)
import Html.Events exposing (onClick)


type alias Attribute msg =
    InternalAttribute msg -> InternalAttribute msg


type alias InternalAttribute msg =
    { title : Maybe String
    , subtitle : Maybe String
    , edit : Maybe ( String, msg )
    , attributes : Maybe (List (Html.Attribute msg))
    }


emptyAttribute : InternalAttribute msg
emptyAttribute =
    { title = Nothing
    , subtitle = Nothing
    , edit = Nothing
    , attributes = Nothing
    }


processAttribute : attribute -> List (attribute -> attribute) -> attribute
processAttribute initialAttribute attributesList =
    List.foldl (\f attribute -> f attribute) initialAttribute attributesList


{-| Get the card view
-}
card : Namespace -> List (Attribute msg) -> List (Html msg) -> Html msg
card namespace attributesList content =
    let
        class =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace

        attribute =
            processAttribute emptyAttribute attributesList
    in
    div (class [ "Card" ] :: (attribute.attributes |> Maybe.withDefault []))
        [ headerView namespace attribute
        , editView namespace attribute
        , div [ class [ "CardBody" ] ] content
        ]


{-| Get the success card view
-}
successCard : Namespace -> Html msg -> List (Attribute msg) -> List (Html msg) -> Html msg
successCard namespace icon attrs content =
    let
        class =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace

        attribute =
            processAttribute emptyAttribute attrs
    in
    div (class [ "Card" ] :: (attribute.attributes |> Maybe.withDefault []))
        [ icon
        , headerView namespace attribute
        , editView namespace attribute
        , div [ class [ "CardBody" ] ] content
        ]


headerView : Namespace -> InternalAttribute msg -> Html msg
headerView namespace attribute =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        subtitleHtml =
            attribute.subtitle
                |> Maybe.map (\subtitleText -> span [ class [ "CardSubtitle" ] ] [ text subtitleText ])
                |> Maybe.withDefault (text "")
    in
    attribute.title
        |> Maybe.map
            (\cardTitle ->
                div [ class [ "CardHeader" ] ]
                    [ span [ class [ "CardTitle" ] ] [ text cardTitle ]
                    , subtitleHtml
                    ]
            )
        |> Maybe.withDefault (div [ class [ "CardHeader" ] ] [ subtitleHtml ])


editView : Namespace -> InternalAttribute msg -> Html msg
editView namespace attribute =
    let
        class =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    attribute.edit
        |> Maybe.map
            (\( text, msg ) ->
                div [ class [ "CardEditButton" ] ]
                    [ Button.primarySmall { namespace = namespace, attributes = [ onClick msg ], text = text }
                    ]
            )
        |> Maybe.withDefault (text "")



-- Attribute


{-| Get the title attribute
-}
title : String -> Attribute msg
title titleText =
    \attribute -> { attribute | title = Just titleText }


{-| Get the subtitle attribute
-}
subtitle : String -> Attribute msg
subtitle subtitleText =
    \attribute -> { attribute | subtitle = Just subtitleText }


{-| Get the edit attribute
-}
edit : String -> msg -> Attribute msg
edit text msg =
    \attribute -> { attribute | edit = Just ( text, msg ) }


{-| Get the attributes attribute
-}
attributes : List (Html.Attribute msg) -> Attribute msg
attributes htmlAttributes =
    \attribute -> { attribute | attributes = Just htmlAttributes }


{-| Get the none attribute
-}
none : Attribute msg
none =
    identity
