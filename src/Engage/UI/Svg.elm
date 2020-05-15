module Engage.UI.Svg exposing
    ( chevron
    , confirmation
    , error
    , fax
    , info
    , mail
    , mobilePhone
    , phone
    , remove
    , upload
    , warning
    )

import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (MessageType(..))
import Engage.Svg.CssHelpers as CssHelpers
import Engage.UI.Message.Css exposing (Class(..))
import Engage.UI.Svg.Css exposing (Class(..))
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


error : { a | namespace : Namespace } -> List (Html.Attribute msg) -> Html msg
error { namespace } attributes =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> CssHelpers.withNamespace
    in
    svg ([ height "24", width "24", viewBox "0 0 24 24", class [ toString (Icon Error), toString SvgPhone ] ] ++ attributes)
        [ Svg.path [ d "M0 0h24v24H0z", fill "none" ]
            []
        , Svg.path [ d "M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 15h-2v-2h2v2zm0-4h-2V7h2v6z" ]
            []
        ]


confirmation : { a | namespace : Namespace } -> List (Html.Attribute msg) -> Html msg
confirmation { namespace } attributes =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> CssHelpers.withNamespace
    in
    svg ([ height "24", width "24", viewBox "0 0 24 24", class [ toString (Icon Confirmation), toString SvgBool ] ] ++ attributes)
        [ Svg.path [ d "M0 0h24v24H0z", fill "none" ]
            []
        , Svg.path [ d "M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm-2 15l-5-5 1.41-1.41L10 14.17l7.59-7.59L19 8l-9 9z" ]
            []
        ]


warning : { a | namespace : Namespace } -> List (Html.Attribute msg) -> Html msg
warning { namespace } attributes =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> CssHelpers.withNamespace
    in
    svg ([ height "24", width "24", viewBox "0 0 24 24", class [ Icon Warning ] ] ++ attributes)
        [ Svg.path [ d "M0 0h24v24H0z", fill "none" ]
            []
        , Svg.path [ d "M1 21h22L12 2 1 21zm12-3h-2v-2h2v2zm0-4h-2v-4h2v4z" ]
            []
        ]


info : { a | namespace : Namespace } -> List (Html.Attribute msg) -> Html msg
info { namespace } attributes =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> CssHelpers.withNamespace
    in
    svg ([ height "24", width "24", viewBox "0 0 24 24", class [ Icon Info ] ] ++ attributes)
        [ Svg.path [ d "M0 0h24v24H0z", fill "none" ]
            []
        , Svg.path [ d "M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 17h-2v-2h2v2zm2.07-7.75l-.9.92C13.45 12.9 13 13.5 13 15h-2v-.5c0-1.1.45-2.1 1.17-2.83l1.24-1.26c.37-.36.59-.86.59-1.41 0-1.1-.9-2-2-2s-2 .9-2 2H8c0-2.21 1.79-4 4-4s4 1.79 4 4c0 .88-.36 1.68-.93 2.25z" ]
            []
        ]


chevron : { a | namespace : Namespace } -> List (Html.Attribute msg) -> Html msg
chevron { namespace } attributes =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> CssHelpers.withNamespace
    in
    svg ([ height "11.2", width "16", viewBox "0 0 16 11.2", class [ Chevron ] ] ++ attributes)
        [ Svg.title [] [ text "chevron" ]
        , Svg.path [ d "M 16,3.2 12.8,0 8,4.8 3.2,0 0,3.2 l 8,8 z" ] []
        ]


mail : Namespace -> String -> List (Html.Attribute msg) -> Html msg
mail namespace titleText attributes =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> CssHelpers.withNamespace
    in
    svg ([ fill "#000000", height "24", viewBox "0 0 24 24", width "24", class [ SvgMail ] ] ++ attributes)
        [ Svg.title [] [ Svg.text titleText ]
        , Svg.path [ d "M0 0h24v24H0z", fill "none" ]
            []
        , Svg.path [ d "M20 4H4c-1.1 0-1.99.9-1.99 2L2 18c0 1.1.9 2 2 2h16c1.1 0 2-.9 2-2V6c0-1.1-.9-2-2-2zm0 14H4V8l8 5 8-5v10zm-8-7L4 6h16l-8 5z" ]
            []
        ]


phone : Namespace -> String -> List (Html.Attribute msg) -> Html msg
phone namespace titleText attributes =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> CssHelpers.withNamespace
    in
    svg ([ fill "#000000", height "24", viewBox "0 0 24 24", width "24", class [ SvgPhone ] ] ++ attributes)
        [ Svg.title [] [ Svg.text titleText ]
        , Svg.path [ d "M0 0h24v24H0z", fill "none" ]
            []
        , Svg.path [ d "M6.62 10.79c1.44 2.83 3.76 5.14 6.59 6.59l2.2-2.2c.27-.27.67-.36 1.02-.24 1.12.37 2.33.57 3.57.57.55 0 1 .45 1 1V20c0 .55-.45 1-1 1-9.39 0-17-7.61-17-17 0-.55.45-1 1-1h3.5c.55 0 1 .45 1 1 0 1.25.2 2.45.57 3.57.11.35.03.74-.25 1.02l-2.2 2.2z" ]
            []
        ]


mobilePhone : Namespace -> String -> List (Html.Attribute msg) -> Html msg
mobilePhone namespace titleText attributes =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> CssHelpers.withNamespace
    in
    svg ([ fill "#000000", height "24", viewBox "0 0 24 24", width "24", class [ SvgMobilePhone ] ] ++ attributes)
        [ Svg.title [] [ Svg.text titleText ]
        , Svg.path [ d "M0 0h24v24H0z", fill "none" ]
            []
        , Svg.path [ d "M17 1.01L7 1c-1.1 0-1.99.9-1.99 2v18c0 1.1.89 2 1.99 2h10c1.1 0 2-.9 2-2V3c0-1.1-.9-1.99-2-1.99zM17 19H7V5h10v14z" ]
            []
        ]


fax : Namespace -> String -> List (Html.Attribute msg) -> Html msg
fax namespace titleText attributes =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> CssHelpers.withNamespace
    in
    svg ([ fill "#000000", height "24", viewBox "0 0 24 24", width "24", class [ SvgFax ] ] ++ attributes)
        [ Svg.title []
            [ Svg.text titleText ]
        , Svg.path [ d "M11,6H16V8H11V6M8,9V3H19V9A3,3 0 0,1 22,12V18H19V21H8V18H7V9H8M10,5V9H17V5H10M10,15V19H17V15H10M19,11A1,1 0 0,0 18,12A1,1 0 0,0 19,13A1,1 0 0,0 20,12A1,1 0 0,0 19,11M4,9H5A1,1 0 0,1 6,10V17A1,1 0 0,1 5,18H4A2,2 0 0,1 2,16V11A2,2 0 0,1 4,9Z" ]
            []
        ]


upload : Namespace -> String -> List (Html.Attribute msg) -> Html msg
upload namespace titleText attributes =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> CssHelpers.withNamespace
    in
    svg ([ fill "#000000", height "24", viewBox "0 0 24 24", width "24", class [ SvgUpload ] ] ++ attributes)
        [ Svg.title []
            [ Svg.text titleText ]
        , Svg.path [ d "M0 0h24v24H0z", fill "none" ]
            []
        , Svg.path [ d "M9 16h6v-6h4l-7-7-7 7h4zm-4 2h14v2H5z" ]
            []
        ]


remove : Namespace -> String -> List (Html.Attribute msg) -> Html msg
remove namespace titleText attributes =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> CssHelpers.withNamespace
    in
    svg ([ fill "#000000", height "24", viewBox "0 0 24 24", width "24", class [ SvgRemove ] ] ++ attributes)
        [ Svg.title []
            [ Svg.text titleText ]
        , Svg.path [ d "M19 6.41L17.59 5 12 10.59 6.41 5 5 6.41 10.59 12 5 17.59 6.41 19 12 13.41 17.59 19 19 17.59 13.41 12z" ]
            []
        , Svg.path [ d "M0 0h24v24H0z", fill "none" ]
            []
        ]
