module Engage.UI.Accordion exposing
    ( State
    , accordionRadioList
    , initialState
    )

import Dict exposing (Dict)
import Engage.CssHelpers
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Class(FormControl), Importance(..), Size(..))
import Engage.UI.Button as Button
import Engage.UI.Error as Error exposing (Status)
import Engage.UI.FormControl as FormControl
import Engage.UI.Message as Message
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown


type State
    = State StateData


type alias StateData =
    { message : Message.State
    , collapse : Dict String CollapseState
    }


type CollapseState
    = Collapsed
    | Expanded


unwrap : State -> StateData
unwrap (State stateData) =
    stateData


initialState : State
initialState =
    State
        { message = Message.initialState
        , collapse = Dict.empty
        }


reset : State
reset =
    initialState


accordionRadioList :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , requiredText : Maybe String
    , onChange : { onlyStateChange : Bool } -> State -> String -> msg
    , status : Status
    , items : List { id : String, text : String, description : String }
    , accordionExpandButtonText : String
    }
    -> State
    -> String
    -> Html msg
accordionRadioList ({ namespace, labelText, onChange, status, items, accordionExpandButtonText } as args) state selectedValue =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        namespacedId =
            Namespace.toString namespace ++ args.id

        stateData =
            unwrap state

        onValidationStateChange validationState =
            onChange { onlyStateChange = True } (State { stateData | message = validationState }) selectedValue
    in
    FormControl.groupFormControl
        { namespace = args.namespace
        , size = Large
        , id = args.id
        , labelText = args.labelText
        , helpText = args.helpText
        , requiredText = args.requiredText
        , status = args.status
        , onValidationStateChange = onValidationStateChange
        }
        stateData.message
        (div [ class [ "AccordionList" ] ]
            (items
                |> List.map
                    (toAccordionRadio
                        { id = args.id
                        , namespace = namespace
                        , labelText = labelText
                        , onChange = onChange
                        , accordionExpandButtonText = accordionExpandButtonText
                        }
                        state
                        selectedValue
                    )
            )
        )


toggleCollapse : String -> Dict String CollapseState -> Dict String CollapseState
toggleCollapse id dict =
    case Dict.get id dict of
        Just Collapsed ->
            Dict.insert id Expanded dict

        Just Expanded ->
            Dict.insert id Collapsed dict

        Nothing ->
            Dict.insert id Expanded dict


toAccordionRadio : { id : String, namespace : Namespace, onChange : { onlyStateChange : Bool } -> State -> String -> msg, labelText : String, accordionExpandButtonText : String } -> State -> String -> { id : String, text : String, description : String } -> Html msg
toAccordionRadio { id, namespace, onChange, labelText, accordionExpandButtonText } ((State stateData) as state) selectedValue item =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        radioInputId =
            Namespace.toString namespace ++ "RadioInput" ++ id ++ item.id

        defaultOptions =
            Markdown.defaultOptions

        expandButtonHandler =
            onChange { onlyStateChange = True } (State { stateData | collapse = toggleCollapse item.id stateData.collapse }) selectedValue

        radioHandler =
            onChange { onlyStateChange = False } state item.id

        accordionState =
            case Dict.get item.id stateData.collapse of
                Just Collapsed ->
                    "AccordionCollapsed"

                Just Expanded ->
                    "AccordionExpanded"

                Nothing ->
                    "AccordionCollapsed"
    in
    div [ class [ "Accordion" ] ]
        [ div [ class [ "AccordionHeader" ] ]
            [ label [ class [ "RadioContainer" ] ]
                [ input
                    [ type_ "radio"
                    , Html.Attributes.id radioInputId
                    , name labelText
                    , value item.id
                    , class [ "RadioInput" ]
                    , onClick radioHandler
                    , checked (selectedValue == item.id)
                    ]
                    []
                , span [] [ Html.text item.text ]
                ]
            , Button.divert
                { namespace = namespace
                , text = accordionExpandButtonText
                , attributes =
                    [ onClick expandButtonHandler ]
                }
            ]
        , Markdown.toHtml [ class [ "AccordionBody-" ++ accordionState ] ] item.description
        ]
