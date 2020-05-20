module Engage.UI.Error exposing
    ( Status(..)
    , error, errorLocalized, inlineError, isError, isNone, localizeStatus, merge
    )

{-| UI.Error

@docs Status

@docs error, errorLocalized, inlineError, isError, isNone, localizeStatus, merge

-}

import Engage.CssHelpers
import Engage.Html.Extra as HtmlExtra
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.UI.List as List
import Engage.UI.Message as Message
import Engage.UI.MessageType as MessageType
import Html exposing (..)



-- TYPES


{-| The Status type
-}
type Status
    = Unknown
    | None { infos : List String }
    | Error { reasons : List String }
    | Ok


{-| Localize the Status
-}
localizeStatus : (String -> String) -> Status -> Status
localizeStatus localizer status =
    case status of
        Unknown ->
            Unknown

        None { infos } ->
            None { infos = infos |> List.map localizer }

        Error { reasons } ->
            Error { reasons = reasons |> List.map localizer }

        Ok ->
            Ok


{-| Merge two statuses
-}
merge : Status -> Status -> Status
merge a b =
    case ( a, b ) of
        ( Error reasonsA, Error reasonsB ) ->
            Error { reasons = reasonsA.reasons ++ reasonsB.reasons }

        ( Error reasons, _ ) ->
            Error reasons

        ( _, Error reasons ) ->
            Error reasons

        ( None infosA, None infosB ) ->
            None { infos = infosA.infos ++ infosB.infos }

        ( None infos, _ ) ->
            None infos

        ( _, None infos ) ->
            None infos

        ( Unknown, _ ) ->
            Unknown

        ( _, Unknown ) ->
            Unknown

        ( Ok, Ok ) ->
            Ok



-- VIEWS


{-| Get the error view
-}
error : { namespace : Namespace } -> Status -> Html msg
error { namespace } status =
    errorLocalized { namespace = namespace, localize = identity } status


{-| Get the error localized view
-}
errorLocalized : { namespace : Namespace, localize : String -> String } -> Status -> Html msg
errorLocalized { namespace, localize } status =
    case status of
        Unknown ->
            HtmlExtra.none

        None { infos } ->
            if List.isEmpty infos then
                HtmlExtra.none

            else
                Message.message
                    { namespace = namespace
                    , messageType = MessageType.Info
                    }
                    (List.map (localize >> Html.text) infos)

        Error { reasons } ->
            Message.message
                { namespace = namespace
                , messageType = MessageType.Error
                }
                (List.map (localize >> Html.text) reasons)

        Ok ->
            Message.message
                { namespace = namespace
                , messageType = MessageType.Confirmation
                }
                []


{-| Get the inline error view
-}
inlineError :
    { namespace : Namespace
    , status : Status
    , onChange : Message.State -> msg
    }
    -> Message.State
    -> Html msg
inlineError { namespace, status, onChange } state =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    case status of
        Unknown ->
            HtmlExtra.none

        None { infos } ->
            if List.isEmpty infos then
                HtmlExtra.none

            else
                Message.controlMessage
                    { namespace = namespace
                    , messageType = MessageType.Confirmation
                    , onChange = onChange
                    }
                    state
                    (List.list { namespace = namespace }
                        (List.map (Html.text >> List.singleton) infos)
                    )

        Error { reasons } ->
            Message.controlMessage
                { namespace = namespace
                , messageType = MessageType.Error
                , onChange = onChange
                }
                state
                (List.list { namespace = namespace }
                    (List.map (Html.text >> List.singleton) reasons)
                )

        Ok ->
            Message.controlMessage
                { namespace = namespace
                , messageType = MessageType.Confirmation
                , onChange = onChange
                }
                state
                HtmlExtra.none



-- Helpers


{-| Check if the Status is None
-}
isNone : Status -> Maybe Bool
isNone result =
    case result of
        Unknown ->
            Nothing

        None _ ->
            Just True

        Ok ->
            Just False

        Error _ ->
            Just False


{-| Check if the Status is Error
-}
isError : Status -> Maybe Bool
isError result =
    case result of
        Unknown ->
            Nothing

        None _ ->
            Just False

        Ok ->
            Just False

        Error _ ->
            Just True
