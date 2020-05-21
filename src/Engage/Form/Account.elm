module Engage.Form.Account exposing
    ( Attribute
    , address, edit, name, none, phone, view
    )

{-| Form.Account

@docs Attribute

@docs address, edit, name, none, phone, view

-}

import Engage.CssHelpers
import Engage.Entity.Address exposing (Address, Countries, RegionsCountry)
import Engage.Form.Address
import Engage.Form.FormAction as FormAction
import Engage.Html.Extra as HtmlExtra
import Engage.Localization as Localization exposing (Localization)
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.UI.Attribute as Attribute
import Engage.UI.Button as Button
import Engage.UI.Info as Info
import Html exposing (..)
import Html.Events exposing (onClick)
import String


type alias InternalAttribute msg =
    { name : Maybe String
    , phone : Maybe ( String, String )
    , address : Maybe ( String, Address )
    , edit : Maybe ( String, msg )
    }


emptyAttribute : InternalAttribute msg
emptyAttribute =
    { name = Nothing
    , phone = Nothing
    , address = Nothing
    , edit = Nothing
    }


{-| The Attribute type
-}
type alias Attribute msg =
    InternalAttribute msg -> InternalAttribute msg


{-| Get the none Attribute
-}
none : Attribute msg
none =
    \attribute -> attribute


{-| Get the name Attribute
-}
name : String -> Attribute msg
name text =
    \attribute -> { attribute | name = Just text }


{-| Get the address Attribute
-}
address : String -> Address -> Attribute msg
address label addressData =
    \attribute -> { attribute | address = Just ( label, addressData ) }


{-| Get the phone Attribute
-}
phone : String -> String -> Attribute msg
phone label text =
    \attribute -> { attribute | phone = Just ( label, text ) }


{-| Get the edit Attribute
-}
edit : String -> msg -> Attribute msg
edit text msg =
    \attribute -> { attribute | edit = Just ( text, msg ) }


{-| Get the view
-}
view : { args | namespace : Namespace, localization : Localization, countries : Countries, regions : RegionsCountry } -> List (Attribute msg) -> List (Html msg) -> Html msg
view ({ namespace, localization } as args) attributes additionalContents =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        attribute =
            Attribute.process emptyAttribute attributes
    in
    div [ class [ "Account" ] ]
        [ div [ class [ "AccountInfo" ] ]
            [ div [ class [ "AccountHeader" ] ]
                [ nameView namespace attribute
                , phoneView namespace attribute
                ]
            , div [ class [ "AccountBody" ] ] (addressView args attribute :: additionalContents)
            , actionView namespace attribute
            ]
        ]


nameView : Namespace -> InternalAttribute msg -> Html msg
nameView namespace attribute =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        nameValue =
            attribute.name |> Maybe.withDefault ""
    in
    if nameValue |> String.trim |> String.isEmpty then
        HtmlExtra.none

    else
        h3 [ class [ "AccountName" ] ] [ text nameValue ]


addressView : { args | namespace : Namespace, localization : Localization, countries : Countries, regions : RegionsCountry } -> InternalAttribute msg -> Html msg
addressView ({ namespace, localization } as args) attribute =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        isEmpty =
            Engage.Form.Address.isEmpty

        do label addressValue =
            if isEmpty addressValue then
                HtmlExtra.none

            else
                Info.multiple namespace (Info.getLabel label) [ Engage.Form.Address.view args addressValue ]
    in
    attribute.address
        |> Maybe.map (\( label, addressValue ) -> do label addressValue)
        |> Maybe.withDefault HtmlExtra.none


phoneView : Namespace -> InternalAttribute msg -> Html msg
phoneView namespace attribute =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    attribute.phone
        |> Maybe.map (\( label, phoneValue ) -> div [ class [ "AccountPhone" ] ] [ Info.phone namespace (Info.getLabel label) phoneValue ])
        |> Maybe.withDefault HtmlExtra.none


actionView : Namespace -> InternalAttribute msg -> Html msg
actionView namespace attribute =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    FormAction.formAction namespace
        [ attribute.edit
            |> Maybe.map
                (\( text, msg ) ->
                    div [ class [ "AccountEditButton" ] ]
                        [ Button.primarySmall
                            { namespace = namespace
                            , attributes = [ onClick msg ]
                            , text = text
                            }
                        ]
                )
            |> Maybe.withDefault HtmlExtra.none
        ]
        []
