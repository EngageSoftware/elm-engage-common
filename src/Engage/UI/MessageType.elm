module Engage.UI.MessageType exposing (MessageType(..), toClass)

import Engage.Styles.Class


type MessageType
    = Confirmation
    | Error
    | Warning
    | Info


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
