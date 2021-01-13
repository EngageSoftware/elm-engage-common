module Engage.UI.Tooltip exposing
    ( State(..)
    , initialState
    , tooltip
    )

import Engage.CssHelpers
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.UI.MessageType as MessageType exposing (MessageType(..))
import Html exposing (..)


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
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    case state of
        Hidden ->
            div [ class [ "Tooltip-" ++ MessageType.toString messageType ++ "-Hidden" ] ] [ contents ]

        Visible ->
            div [ class [ "Tooltip-" ++ MessageType.toString messageType ++ "-Visible" ] ] [ contents ]
