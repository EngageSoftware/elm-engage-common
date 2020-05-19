module Engage.UI.MessageType exposing (MessageType(..), toClass)

{-| MessageType

@docs MessageType

@docs toClass

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
