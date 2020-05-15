module Engage.Form.Profile exposing
    ( Attribute
    , address
    , avatar
    , birthDate
    , birthDateMonth
    , birthDateYear
    , edit
    , editAccountLink
    , email
    , fax
    , firstName
    , gender
    , lastName
    , mobilePhone
    , none
    , phone
    , title
    , view
    )

import Date exposing (Date)
import Date.Extra.Config.Config_en_us
import Date.Extra.Format
import Engage.Entity.Address exposing (Address, Countries, RegionsCountry)
import Engage.Entity.Gender as Gender exposing (Gender)
import Engage.Form.Address
import Engage.Form.FormAction as FormAction
import Engage.Form.Profile.Css exposing (Class(..))
import Engage.ListItem as ListItem exposing (ListItem)
import Engage.Localization as Localization exposing (Localization)
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.UI.Attribute as Attribute
import Engage.UI.Button as Button
import Engage.UI.Info as Info
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.CssHelpers
import Html.Events exposing (onClick)
import String


type alias InternalAttribute msg =
    { avatar : Maybe String
    , firstName : Maybe String
    , lastName : Maybe String
    , title : Maybe String
    , email : Maybe ( String, String )
    , phone : Maybe ( String, String )
    , mobilePhone : Maybe ( String, String )
    , fax : Maybe ( String, String )
    , edit : Maybe ( String, msg )
    , address : Maybe ( String, Address )
    , gender : Maybe ( String, Gender )
    , birthDate : Maybe ( String, Maybe Date )
    , birthDateYear : Maybe ( String, Maybe ListItem )
    , birthDateMonth : Maybe ( String, Maybe ListItem )
    , editAccountLink : Maybe ( String, String )
    }


emptyAttribute : InternalAttribute msg
emptyAttribute =
    { avatar = Nothing
    , firstName = Nothing
    , lastName = Nothing
    , title = Nothing
    , email = Nothing
    , phone = Nothing
    , mobilePhone = Nothing
    , fax = Nothing
    , edit = Nothing
    , address = Nothing
    , gender = Nothing
    , birthDate = Nothing
    , birthDateYear = Nothing
    , birthDateMonth = Nothing
    , editAccountLink = Nothing
    }


type alias Attribute msg =
    InternalAttribute msg -> InternalAttribute msg


none : Attribute msg
none =
    \attribute -> attribute


firstName : String -> Attribute msg
firstName text =
    \attribute -> { attribute | firstName = Just text }


title : String -> Attribute msg
title text =
    \attribute -> { attribute | title = Just text }


address : String -> Address -> Attribute msg
address label addressData =
    \attribute -> { attribute | address = Just ( label, addressData ) }


lastName : String -> Attribute msg
lastName text =
    \attribute -> { attribute | lastName = Just text }


avatar : String -> Attribute msg
avatar text =
    \attribute -> { attribute | avatar = Just text }


email : String -> String -> Attribute msg
email label text =
    \attribute -> { attribute | email = Just ( label, text ) }


phone : String -> String -> Attribute msg
phone label text =
    \attribute -> { attribute | phone = Just ( label, text ) }


mobilePhone : String -> String -> Attribute msg
mobilePhone label text =
    \attribute -> { attribute | mobilePhone = Just ( label, text ) }


fax : String -> String -> Attribute msg
fax label text =
    \attribute -> { attribute | fax = Just ( label, text ) }


gender : String -> Gender -> Attribute msg
gender label value =
    \attribute -> { attribute | gender = Just ( label, value ) }


birthDate : String -> Maybe Date -> Attribute msg
birthDate label date =
    \attribute -> { attribute | birthDate = Just ( label, date ) }


birthDateYear : String -> Maybe ListItem -> Attribute msg
birthDateYear label item =
    \attribute -> { attribute | birthDateYear = Just ( label, item ) }


birthDateMonth : String -> Maybe ListItem -> Attribute msg
birthDateMonth label item =
    \attribute -> { attribute | birthDateMonth = Just ( label, item ) }


edit : String -> msg -> Attribute msg
edit text msg =
    \attribute -> { attribute | edit = Just ( text, msg ) }


editAccountLink : String -> String -> Attribute msg
editAccountLink text value =
    \attribute -> { attribute | editAccountLink = Just ( text, value ) }


view : { args | namespace : Namespace, localization : Localization, countries : Countries, regions : RegionsCountry } -> List (Attribute msg) -> List (Html msg) -> Html msg
view ({ namespace, localization } as args) attributes additionalContents =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        attribute =
            Attribute.process emptyAttribute attributes
    in
    div [ class [ Profile ] ]
        [ avatarView namespace attribute
        , div [ class [ ProfileInfo ] ]
            [ div [ class [ ProfileHeader ] ]
                [ nameView namespace attribute
                , titleView namespace attribute
                , emailView namespace attribute
                , phoneView namespace attribute
                , mobilePhoneView namespace attribute
                , faxView namespace attribute
                ]
            , div [ class [ ProfileBody ] ]
                ([ genderView namespace attribute
                 , birthDateView namespace localization attribute
                 , birthDateMonthView namespace attribute
                 , birthDateYearView namespace attribute
                 , addressView args attribute
                 ]
                    ++ additionalContents
                )
            , actionView namespace attribute
            ]
        ]


genderView : Namespace -> InternalAttribute msg -> Html msg
genderView namespace attribute =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        do label gender =
            if String.isEmpty gender then
                text ""

            else
                Info.multiple namespace (Info.label label) [ text gender ]
    in
    attribute.gender
        |> Maybe.map (\( label, gender ) -> do label (Gender.toString gender))
        |> Maybe.withDefault (text "")


birthDateView : Namespace -> Localization -> InternalAttribute msg -> Html msg
birthDateView namespace localization attribute =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        dateFormat =
            Localization.localizeStringWithDefault "%B %e, %Y" "BirthDateFormat" { localization = localization }

        do label maybeDate =
            case maybeDate of
                Nothing ->
                    text ""

                Just date ->
                    Info.multiple namespace (Info.label label) [ text (Date.Extra.Format.format Date.Extra.Config.Config_en_us.config dateFormat date) ]
    in
    attribute.birthDate
        |> Maybe.map (\( label, date ) -> do label date)
        |> Maybe.withDefault (text "")


birthDateYearView : Namespace -> InternalAttribute msg -> Html msg
birthDateYearView namespace attribute =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        do label maybeItem =
            case maybeItem of
                Nothing ->
                    text ""

                Just item ->
                    Info.multiple namespace (Info.label label) [ text (toString (Tuple.first item)) ]
    in
    attribute.birthDateYear
        |> Maybe.map (\( label, item ) -> do label item)
        |> Maybe.withDefault (text "")


birthDateMonthView : Namespace -> InternalAttribute msg -> Html msg
birthDateMonthView namespace attribute =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        do label maybeItem =
            case maybeItem of
                Nothing ->
                    text ""

                Just item ->
                    Info.multiple namespace (Info.label label) [ text (toString (Tuple.first item)) ]
    in
    attribute.birthDateMonth
        |> Maybe.map (\( label, item ) -> do label item)
        |> Maybe.withDefault (text "")


nameView : Namespace -> InternalAttribute msg -> Html msg
nameView namespace attribute =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        firstName =
            attribute.firstName |> Maybe.withDefault ""

        lastName =
            attribute.lastName |> Maybe.withDefault ""

        name =
            firstName ++ " " ++ lastName
    in
    if name |> String.trim |> String.isEmpty then
        text ""

    else
        h3 [ class [ ProfileName ] ] [ text name ]


titleView : Namespace -> InternalAttribute msg -> Html msg
titleView namespace attribute =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    attribute.title
        |> Maybe.map (\title -> div [ class [ ProfileTitle ] ] [ text title ])
        |> Maybe.withDefault (text "")


addressView : { args | namespace : Namespace, localization : Localization, countries : Countries, regions : RegionsCountry } -> InternalAttribute msg -> Html msg
addressView ({ namespace, localization } as args) attribute =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        isEmpty =
            Engage.Form.Address.isEmpty

        do label address =
            if isEmpty address then
                text ""

            else
                Info.multiple namespace (Info.label label) [ Engage.Form.Address.view args address ]
    in
    attribute.address
        |> Maybe.map (\( label, address ) -> do label address)
        |> Maybe.withDefault (text "")


avatarView : Namespace -> InternalAttribute msg -> Html msg
avatarView namespace attribute =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    attribute.avatar
        |> Maybe.map
            (\avatar ->
                div
                    [ class
                        (if String.isEmpty avatar then
                            [ ProfileNoAvatar, ProfileAvatar ]

                         else
                            [ ProfileAvatar ]
                        )
                    ]
                    [ img
                        [ src
                            (if String.isEmpty avatar then
                                "/DesktopModules/EngageCore.Participant/images/noprofile.png"

                             else
                                avatar
                            )
                        ]
                        []
                    ]
            )
        |> Maybe.withDefault (text "")


emailView : Namespace -> InternalAttribute msg -> Html msg
emailView namespace attribute =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    attribute.email
        |> Maybe.map (\( label, email ) -> div [ class [ ProfileEmail ] ] [ Info.email namespace (Info.label label) email ])
        |> Maybe.withDefault (text "")


phoneView : Namespace -> InternalAttribute msg -> Html msg
phoneView namespace attribute =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    attribute.phone
        |> Maybe.map (\( label, phone ) -> div [ class [ ProfilePhone ] ] [ Info.phone namespace (Info.label label) phone ])
        |> Maybe.withDefault (text "")


mobilePhoneView : Namespace -> InternalAttribute msg -> Html msg
mobilePhoneView namespace attribute =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    attribute.mobilePhone
        |> Maybe.map (\( label, mobilePhone ) -> div [ class [ ProfileCellphone ] ] [ Info.mobilePhone namespace (Info.label label) mobilePhone ])
        |> Maybe.withDefault (text "")


faxView : Namespace -> InternalAttribute msg -> Html msg
faxView namespace attribute =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    attribute.fax
        |> Maybe.map (\( label, fax ) -> div [ class [ ProfileFax ] ] [ Info.fax namespace (Info.label label) fax ])
        |> Maybe.withDefault (text "")


actionView : Namespace -> InternalAttribute msg -> Html msg
actionView namespace attribute =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    FormAction.formAction namespace
        [ attribute.edit
            |> Maybe.map
                (\( text, msg ) ->
                    div [ class [ ProfileEditButton ] ]
                        [ Button.primarySmall
                            { namespace = namespace
                            , attributes = [ onClick msg ]
                            , text = text
                            }
                        ]
                )
            |> Maybe.withDefault (text "")
        , attribute.editAccountLink
            |> Maybe.map
                (\( text, url ) ->
                    div [ class [ ProfileEditAccountLink ] ]
                        [ a [ href url, Html.Attributes.attribute "class" "dialog-link" ] [ Html.text text ] ]
                )
            |> Maybe.withDefault (text "")
        ]
        []
