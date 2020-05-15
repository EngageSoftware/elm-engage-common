module Engage.UI.Tooltip exposing
    ( State(..)
    , initialState
    , not
    , tooltip
    )

import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class as Css
import Engage.UI.MessageType as MessageType exposing (MessageType(..))
import Engage.UI.Tooltip.Css exposing (Class(..))
import Html exposing (..)
import Html.CssHelpers


type State
    = Visible
    | Hidden


initialState : State
initialState =
    Hidden


not : State -> State
not state =
    case state of
        Visible ->
            Hidden

        Hidden ->
            Visible


tooltip : { namespace : Namespace, messageType : MessageType } -> State -> Html msg -> Html msg
tooltip { namespace, messageType } state contents =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    case state of
        Hidden ->
            div [ class [ Tooltip (MessageType.toClass messageType) Css.Hidden ] ] [ contents ]

        Visible ->
            div [ class [ Tooltip (MessageType.toClass messageType) Css.Visible ] ] [ contents ]
