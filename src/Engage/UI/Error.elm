module Engage.UI.Error exposing
    ( Status(..)
    , error
    , errorLocalized
    , inlineError
    , isError
    , isNone
    , localize
    , merge
    )

import Engage.Html.Extra as HtmlExtra
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.UI.List as List
import Engage.UI.Message as Message
import Engage.UI.MessageType as MessageType
import Html exposing (..)
import Html.CssHelpers



-- TYPES


type Status
    = Unknown
    | None { infos : List String }
    | Error { reasons : List String }
    | Ok


localize : (String -> String) -> Status -> Status
localize localizer status =
    case status of
        Unknown ->
            Unknown

        None { infos } ->
            None { infos = infos |> List.map localizer }

        Error { reasons } ->
            Error { reasons = reasons |> List.map localizer }

        Ok ->
            Ok


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


error : { namespace : Namespace } -> Status -> Html msg
error { namespace } status =
    errorLocalized { namespace = namespace, localize = identity } status


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


inlineError :
    { namespace : Namespace
    , status : Status
    , onChange : Message.State -> msg
    }
    -> Message.State
    -> Html msg
inlineError { namespace, status, onChange } state =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
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
