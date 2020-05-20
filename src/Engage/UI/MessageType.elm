module Engage.UI.MessageType exposing
    ( MessageType(..)
    , toClass, toString
    )

{-| MessageType

@docs MessageType

@docs toClass, toString

-}

import Engage.Styles.Class


{-| The MessageType type
-}
type MessageType
    = Confirmation
    | Error
    | Warning
    | Info


{-| Convert a MesssageType into a class
-}
toClass : MessageType -> Engage.Styles.Class.MessageType
toClass messageType =
    case messageType of
        Confirmation ->
            Engage.Styles.Class.Confirmation

        Error ->
            Engage.Styles.Class.Error

        Warning ->
            Engage.Styles.Class.Warning

        Info ->
            Engage.Styles.Class.Info


{-| Convert a MesssageType into a string
-}
toString : MessageType -> String
toString messageType =
    case messageType of
        Confirmation ->
            "Confirmation"

        Error ->
            "Error"

        Warning ->
            "Warning"

        Info ->
            "Info"
