module Engage.UI.Message exposing
    ( State
    , controlMessage, initialState, inlineMessage, message
    )

{-| UI.Message

@docs State

@docs controlMessage, initialState, inlineMessage, message

-}

import Engage.CssHelpers
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.UI.MessageType exposing (MessageType(..))
import Engage.UI.Svg as Svg
import Engage.UI.Tooltip as Tooltip
import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Events


type alias Config msg =
    { namespace : Namespace
    , messageType : MessageType
    , onChange : State -> msg
    }


{-| The State type
-}
type State
    = State StateData


type alias StateData =
    { tooltip : Tooltip.State
    , isHovered : Bool
    }


{-| Get the initial State
-}
initialState : State
initialState =
    State
        { tooltip = Tooltip.initialState
        , isHovered = False
        }



-- VIEWS


{-| Get a message
-}
message : { namespace : Namespace, messageType : MessageType } -> List (Html msg) -> Html msg
message { namespace, messageType } contents =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    Html.div [ class [ "Message-" ++ getMessageTypeString messageType ] ] contents


{-| Get an inline message
-}
inlineMessage : { namespace : Namespace, messageType : MessageType } -> List (Html msg) -> Html msg
inlineMessage { namespace, messageType } contents =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    Html.span [ class [ "InlineMessage-" ++ getMessageTypeString messageType ] ] contents


{-| Get a control message
-}
controlMessage : Config msg -> State -> Html msg -> Html msg
controlMessage { namespace, messageType, onChange } (State stateData) contents =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        ifNotHovered state =
            State
                { stateData
                    | tooltip =
                        if stateData.isHovered then
                            stateData.tooltip

                        else
                            state
                }
    in
    Html.div [ class [ "ControlMessage-" ++ getMessageTypeString messageType ] ]
        [ icon
            { namespace = namespace
            , messageType = messageType
            }
            [ Html.Events.onMouseEnter (onChange <| State { stateData | tooltip = Tooltip.Visible, isHovered = True })
            , Html.Events.onMouseLeave (onChange <| State { stateData | tooltip = Tooltip.Hidden, isHovered = False })
            , Html.Events.onFocus (onChange <| ifNotHovered Tooltip.Visible)
            , Html.Events.onBlur (onChange <| ifNotHovered Tooltip.Hidden)
            ]
        , Tooltip.tooltip { namespace = namespace, messageType = messageType } stateData.tooltip contents
        ]



-- renderers


icon : { namespace : Namespace, messageType : MessageType } -> List (Html.Attribute msg) -> Html msg
icon { namespace, messageType } attributes =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    button
        ([ class [ "IconContainer" ]
         , type_ "button"
         ]
            ++ attributes
        )
        [ case messageType of
            Confirmation ->
                Svg.confirmation { namespace = namespace } []

            Error ->
                Svg.error { namespace = namespace } []

            Warning ->
                Svg.warning { namespace = namespace } []

            Info ->
                Svg.info { namespace = namespace } []
        ]



-- Helpers


getMessageTypeString : MessageType -> String
getMessageTypeString messageType =
    case messageType of
        Confirmation ->
            "Confirmation"

        Error ->
            "Error"

        Warning ->
            "Warning"

        Info ->
            "Info"
