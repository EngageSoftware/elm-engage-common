module Engage.UI.Info exposing (bool, email, fax, group, info, label, mobilePhone, multiple, phone)

{-| UI.Info

@docs bool, email, fax, group, info, label, mobilePhone, multiple, phone

-}

import Engage.Bool
import Engage.CssHelpers
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.UI.Info.Css exposing (Class(..))
import Engage.UI.Svg as Svg
import Html exposing (..)



-- LABEL


type Label
    = Label String


{-| Get a Label from a String
-}
label : String -> Label
label =
    Label



-- VIEW


{-| Get the group view
-}
group : Namespace -> List (Html msg) -> Html msg
group namespace content =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    div [ class [ InfoGroup ] ] content


{-| Get the info view
-}
info : Namespace -> Label -> String -> Html msg
info namespace label text =
    let
        namespaced =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    div [ namespaced.class [ Info ] ]
        [ labelView namespace label
        , textView namespace text
        ]


{-| Get the mutlitple view
-}
multiple : Namespace -> Label -> List (Html msg) -> Html msg
multiple namespace label content =
    let
        namespaced =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    div [ namespaced.class [ Info ] ]
        [ labelView namespace label
        , multipleTextView namespace content
        ]


{-| Get the mobilePhone view
-}
mobilePhone : Namespace -> Label -> String -> Html msg
mobilePhone namespace (Label label) mobilePhone =
    let
        namespaced =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    if String.isEmpty <| String.trim <| mobilePhone then
        text ""

    else
        div [ namespaced.class [ Info, InfoMobilePhone ] ]
            [ Svg.mobilePhone namespace label []
            , textView namespace mobilePhone
            ]


{-| Get the phone view
-}
phone : Namespace -> Label -> String -> Html msg
phone namespace (Label label) phoneNumber =
    let
        namespaced =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    if String.isEmpty <| String.trim <| phoneNumber then
        Html.text ""

    else
        div [ namespaced.class [ Info, InfoPhone ] ]
            [ Svg.phone namespace label []
            , textView namespace phoneNumber
            ]


{-| Get the email view
-}
email : Namespace -> Label -> String -> Html msg
email namespace (Label label) emailAddress =
    let
        namespaced =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    if String.isEmpty <| String.trim <| emailAddress then
        Html.text ""

    else
        div [ namespaced.class [ Info, InfoMail ] ]
            [ Svg.mail namespace label []
            , textView namespace emailAddress
            ]


{-| Get the fax view
-}
fax : Namespace -> Label -> String -> Html msg
fax namespace (Label label) faxNumber =
    let
        namespaced =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    if String.isEmpty <| String.trim <| faxNumber then
        Html.text ""

    else
        div [ namespaced.class [ Info, InfoFax ] ]
            [ Svg.fax namespace label []
            , textView namespace faxNumber
            ]


{-| Get the bool view
-}
bool : Namespace -> Label -> Bool -> Html msg
bool namespace (Label label) value =
    let
        namespaced =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    div [ namespaced.class [ Info, InfoBool ] ]
        [ value
            |> Engage.Bool.true (Svg.confirmation { namespace = namespace } [])
            |> Engage.Bool.false (Svg.error { namespace = namespace } [])
        , textView namespace label
        ]



-- HELPER VIEW


labelView : Namespace -> Label -> Html msg
labelView namespace (Label labelText) =
    let
        namespaced =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    div [ namespaced.class [ InfoTitle ] ] [ text labelText ]


textView : Namespace -> String -> Html msg
textView namespace text =
    let
        namespaced =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    div [ namespaced.class [ InfoContent ] ] [ Html.text text ]


multipleTextView : Namespace -> List (Html msg) -> Html msg
multipleTextView namespace content =
    let
        namespaced =
            Engage.CssHelpers.withNamespace <| Namespace.toString namespace
    in
    div [ namespaced.class [ InfoContent ] ] content
