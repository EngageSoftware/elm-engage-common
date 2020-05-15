module Engage.UI.FormControl exposing
    ( formControl
    , groupFormControl
    , labelWrapped
    )

import Engage.Html.Extra as HtmlExtra
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Class(FormControl), Size(..))
import Engage.UI.Error as Error exposing (Status(..))
import Engage.UI.Input.Css exposing (Class(..))
import Engage.UI.Message as Message
import Engage.UI.MessageType as MessageType
import Html exposing (..)
import Html.Attributes exposing (for, id, title)
import Html.Attributes.Aria as Aria
import Html.CssHelpers
import String


type alias Args a msg =
    { a
        | namespace : Namespace
        , size : Size
        , id : String
        , labelText : String
        , helpText : String
        , requiredText : Maybe String
        , onValidationStateChange : Message.State -> msg
        , status : Status
    }


formControl : Args a msg -> Message.State -> Html msg -> Html msg
formControl args state element =
    let
        { class } =
            args.namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    div [ class [ FormControl args.size ] ]
        [ label
            [ class [ Label ], for args.id ]
            [ text args.labelText, requiredIndicator args ]
        , helpMessage args state
        , Error.inlineError
            { namespace = args.namespace
            , status = args.status
            , onChange = args.onValidationStateChange
            }
            state
        , element
        ]


groupFormControl : Args a msg -> Message.State -> Html msg -> Html msg
groupFormControl args state element =
    let
        { class } =
            args.namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        labelId =
            Namespace.toString args.namespace ++ "label" ++ args.id
    in
    div [ class [ FormControl args.size ], Aria.role "group", Aria.ariaLabelledby labelId ]
        [ label [ class [ Label ], for args.id, id labelId ] [ text args.labelText, requiredIndicator args ]
        , helpMessage args state
        , Error.inlineError
            { namespace = args.namespace
            , status = args.status
            , onChange = args.onValidationStateChange
            }
            state
        , element
        ]


labelWrapped : Args a msg -> Message.State -> Html msg -> Html msg -> Html msg
labelWrapped args state element other =
    let
        { class } =
            args.namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    div [ class [ FormControl args.size ] ]
        [ label [ for args.id, class [ LabelWrapped ] ]
            [ span [ class [ Label ] ] [ text args.labelText, requiredIndicator args ]
            , helpMessage args state
            , Error.inlineError
                { namespace = args.namespace
                , status = args.status
                , onChange = args.onValidationStateChange
                }
                state
            , element
            ]
        , other
        ]


requiredIndicator : Args a msg -> Html msg
requiredIndicator args =
    let
        { class } =
            args.namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    args.requiredText
        |> Maybe.map (\required -> span [ class [ Required ], title required ] [ text "*" ])
        |> Maybe.withDefault HtmlExtra.none


helpMessage : Args a msg -> Message.State -> Html msg
helpMessage args state =
    let
        help =
            Message.controlMessage
                { namespace = args.namespace
                , messageType = MessageType.Info
                , onChange = args.onValidationStateChange
                }
                state
                (Html.text args.helpText)

        isHelpTextEmpty =
            args.helpText |> String.trim |> String.isEmpty
    in
    case args.status of
        Unknown ->
            if not isHelpTextEmpty then
                help

            else
                HtmlExtra.none

        None { infos } ->
            if not isHelpTextEmpty && List.isEmpty infos then
                help

            else
                HtmlExtra.none

        Error _ ->
            HtmlExtra.none

        Error.Ok ->
            HtmlExtra.none
