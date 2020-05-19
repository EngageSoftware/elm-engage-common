module Engage.UI.Input exposing
    ( FileInfo
    , PhoneState
    , State
    , bigNumber
    , checkBoxList
    , checkbox
    , checkboxWithAttributes
    , file
    , initialPhoneState
    , initialState
    , number
    , phone
    , radioList
    , reset
    , smallNumber
    , text
    , textArea
    , textWithAttributes
    )

{-| UI.Input

@docs FileInfo, PhoneState, State

@docs bigNumber, checkBoxList, checkbox, checkboxWithAttributes, file, initialPhoneState, initialState, number, phone, radioList, reset, smallNumber, text, textArea, textWithAttributes

-}

import Engage.Entity.PhoneNumber exposing (PhoneNumber)
import Engage.Html.Extra as HtmlExtra
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.String
import Engage.Styles.Class exposing (Class(FormControl), Importance(..), Size(..))
import Engage.UI.Button.Css exposing (Class(..))
import Engage.UI.Error as Error exposing (Status)
import Engage.UI.FormControl as FormControl
import Engage.UI.Input.Css exposing (Class(..))
import Engage.UI.Loading as Loading
import Engage.UI.Message as Message
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.CssHelpers
import Html.Events exposing (..)
import Input.BigNumber
import Input.Number
import Input.Text
import IntlPhoneInput
import IntlPhoneInput.Config
import Json.Decode
import Json.Decode.Pipeline exposing (decode, required, requiredAt)
import Set exposing (Set)


{-| A State type
-}
type State
    = State Message.State


{-| A PhoneState type
-}
type alias PhoneState =
    { message : Message.State
    , phoneInput : IntlPhoneInput.State
    }


{-| Get the initial State
-}
initialState : State
initialState =
    State Message.initialState


{-| Get the initial PhoneState
-}
initialPhoneState : PhoneState
initialPhoneState =
    { message = Message.initialState
    , phoneInput = IntlPhoneInput.initialState
    }


{-| Reset the State
-}
reset : State
reset =
    initialState


{-| Get the phone view
-}
phone :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> PhoneState -> PhoneNumber -> Cmd msg -> msg
    , status : Status
    , requiredText : Maybe String
    }
    -> PhoneState
    -> PhoneNumber
    -> Html msg
phone { namespace, id, labelText, helpText, onChange, status, requiredText } state phoneNumber =
    phoneWithSizeAndAttributes
        { namespace = namespace
        , id = id
        , labelText = labelText
        , helpText = helpText
        , onChange = onChange
        , status = status
        , size = Large
        , requiredText = requiredText
        }
        []
        state
        phoneNumber


{-| Get the phone with size and attributes view
-}
phoneWithSizeAndAttributes :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> PhoneState -> PhoneNumber -> Cmd msg -> msg
    , status : Status
    , size : Size
    , requiredText : Maybe String
    }
    -> List (Html.Attribute msg)
    -> PhoneState
    -> PhoneNumber
    -> Html msg
phoneWithSizeAndAttributes { namespace, id, labelText, helpText, onChange, status, size, requiredText } attributes state phoneNumber =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        safeId =
            Engage.String.toSafeId id

        defaultConfig =
            IntlPhoneInput.Config.configWithId
                safeId
                (\phoneState phoneNumber cmd ->
                    onChange { onlyStateChange = False } { state | phoneInput = phoneState } phoneNumber cmd
                )

        config =
            { defaultConfig | namespace = Namespace.toString namespace }

        onValidationStateChange validationState =
            onChange { onlyStateChange = True } { state | message = validationState } phoneNumber Cmd.none

        inputId =
            IntlPhoneInput.Config.getPhoneNumberInputId config
    in
    FormControl.formControl
        { namespace = namespace
        , size = size
        , id = inputId
        , labelText = labelText
        , helpText = helpText
        , status = status
        , onValidationStateChange = onValidationStateChange
        , requiredText = requiredText
        }
        state.message
        (IntlPhoneInput.customInput
            (class [ Input size ] :: attributes)
            config
            state.phoneInput
            phoneNumber
        )


{-| Get a text view
-}
text :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> State -> String -> msg
    , status : Status
    , requiredText : Maybe String
    }
    -> State
    -> String
    -> Html msg
text args state value =
    textWithAttributes args [] state value


{-| Get a text with attributes view
-}
textWithAttributes :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> State -> String -> msg
    , status : Status
    , requiredText : Maybe String
    }
    -> List (Html.Attribute msg)
    -> State
    -> String
    -> Html msg
textWithAttributes { namespace, id, labelText, helpText, onChange, status, requiredText } attributes state value =
    textWithSizeAndAttributes
        { id = id
        , labelText = labelText
        , helpText = helpText
        , onChange = onChange
        , status = status
        , size = Large
        , namespace = namespace
        , requiredText = requiredText
        }
        attributes
        state
        value


{-| Get a text with size view
-}
textWithSize :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> State -> String -> msg
    , status : Status
    , size : Size
    , requiredText : Maybe String
    }
    -> State
    -> String
    -> Html msg
textWithSize args state value =
    textWithSizeAndAttributes args [] state value


{-| Get a text with size and attributes view
-}
textWithSizeAndAttributes :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> State -> String -> msg
    , status : Status
    , size : Size
    , requiredText : Maybe String
    }
    -> List (Html.Attribute msg)
    -> State
    -> String
    -> Html msg
textWithSizeAndAttributes { namespace, id, labelText, helpText, onChange, status, size, requiredText } attributes state value =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        options =
            Input.Text.defaultOptions (onChange { onlyStateChange = False } state)

        stateData =
            unwrap state

        onValidationStateChange validationState =
            onChange { onlyStateChange = True } (State validationState) value
    in
    FormControl.formControl
        { namespace = namespace
        , size = size
        , id = id
        , labelText = labelText
        , helpText = helpText
        , status = status
        , onValidationStateChange = onValidationStateChange
        , requiredText = requiredText
        }
        stateData
        (Input.Text.input
            options
            (attributes ++ [ Html.Attributes.id id, class [ Input size ] ])
            value
        )


{-| Get a number view
-}
number :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> State -> Maybe Int -> msg
    , status : Status
    , maxValue : Maybe Int
    , minValue : Maybe Int
    , requiredText : Maybe String
    }
    -> State
    -> Maybe Int
    -> Html msg
number { namespace, id, labelText, helpText, onChange, status, maxValue, minValue, requiredText } state value =
    numberWithSize
        { id = id
        , labelText = labelText
        , helpText = helpText
        , onChange = onChange
        , status = status
        , size = Large
        , namespace = namespace
        , maxValue = maxValue
        , minValue = minValue
        , requiredText = requiredText
        }
        state
        value


{-| Get a small number view
-}
smallNumber :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> State -> Maybe Int -> msg
    , status : Status
    , maxValue : Maybe Int
    , minValue : Maybe Int
    , requiredText : Maybe String
    }
    -> State
    -> Maybe Int
    -> Html msg
smallNumber { namespace, id, labelText, helpText, onChange, status, maxValue, minValue, requiredText } state value =
    numberWithSize
        { id = id
        , labelText = labelText
        , helpText = helpText
        , onChange = onChange
        , size = Small
        , status = status
        , namespace = namespace
        , maxValue = maxValue
        , minValue = minValue
        , requiredText = requiredText
        }
        state
        value


{-| Get a number with size view
-}
numberWithSize :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> State -> Maybe Int -> msg
    , status : Status
    , size : Size
    , maxValue : Maybe Int
    , minValue : Maybe Int
    , requiredText : Maybe String
    }
    -> State
    -> Maybe Int
    -> Html msg
numberWithSize { namespace, id, labelText, helpText, onChange, status, size, maxValue, minValue, requiredText } state value =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        options =
            Input.Number.defaultOptions (onChange { onlyStateChange = False } state)

        stateData =
            unwrap state

        onValidationStateChange validationState =
            onChange { onlyStateChange = True } (State validationState) value
    in
    FormControl.formControl
        { namespace = namespace
        , size = size
        , id = id
        , labelText = labelText
        , helpText = helpText
        , onValidationStateChange = onValidationStateChange
        , status = status
        , requiredText = requiredText
        }
        stateData
        (Input.Number.input
            { options | maxValue = maxValue, minValue = minValue }
            [ Html.Attributes.id id, class [ Input size ] ]
            value
        )


{-| Get a big number view
-}
bigNumber :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : State -> String -> msg
    , maxLength : Maybe Int
    , status : Status
    , requiredText : Maybe String
    }
    -> State
    -> String
    -> Html msg
bigNumber ({ namespace, id, labelText, helpText, onChange, status } as args) state value =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        defaultOptions =
            Input.BigNumber.defaultOptions (onChange state)

        options =
            { defaultOptions | maxLength = args.maxLength }

        stateData =
            unwrap state

        onValidationStateChange validationState =
            onChange (State validationState) value
    in
    FormControl.formControl
        { namespace = namespace
        , size = Large
        , id = id
        , labelText = labelText
        , helpText = helpText
        , onValidationStateChange = onValidationStateChange
        , status = status
        , requiredText = args.requiredText
        }
        stateData
        (Input.BigNumber.input
            options
            [ Html.Attributes.id id, class [ Input Large ] ]
            value
        )


{-| Get a text area view
-}
textArea :
    { namespace : Namespace
    , id : String
    , helpText : String
    , labelText : String
    , onChange : { onlyStateChange : Bool } -> State -> String -> msg
    , status : Status
    , requiredText : Maybe String
    }
    -> State
    -> String
    -> Html msg
textArea { namespace, id, labelText, helpText, onChange, status, requiredText } state value =
    textAreaWithSize
        { id = id
        , labelText = labelText
        , onChange = onChange
        , helpText = helpText
        , size = Large
        , status = status
        , namespace = namespace
        , state = state
        , requiredText = requiredText
        }
        value


{-| Get a text area with size view
-}
textAreaWithSize :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> State -> String -> msg
    , status : Status
    , size : Size
    , state : State
    , requiredText : Maybe String
    }
    -> String
    -> Html msg
textAreaWithSize args value =
    let
        { class } =
            args.namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        stateData =
            unwrap args.state

        onInputChange =
            args.onChange { onlyStateChange = False } args.state

        onValidationStateChange validationState =
            args.onChange { onlyStateChange = True } (State validationState) value
    in
    FormControl.formControl
        { namespace = args.namespace
        , size = args.size
        , id = args.id
        , labelText = args.labelText
        , helpText = args.helpText
        , status = args.status
        , onValidationStateChange = onValidationStateChange
        , requiredText = args.requiredText
        }
        stateData
        (Html.textarea
            [ Html.Attributes.id args.id
            , Html.Events.onInput onInputChange
            , class [ TextArea args.size ]
            ]
            [ Html.text value ]
        )


{-| Get a checkbox view
-}
checkbox :
    { namespace : Namespace
    , labelText : String
    , helpText : String
    , onCheck : { onlyStateChange : Bool } -> State -> Bool -> msg
    , status : Status
    , state : State
    , requiredText : Maybe String
    }
    -> Bool
    -> Html msg
checkbox args checked =
    checkboxWithAttributes args [] checked


{-| Get a checkbox with attributes view
-}
checkboxWithAttributes :
    { namespace : Namespace
    , labelText : String
    , helpText : String
    , onCheck : { onlyStateChange : Bool } -> State -> Bool -> msg
    , status : Status
    , state : State
    , requiredText : Maybe String
    }
    -> List (Html.Attribute msg)
    -> Bool
    -> Html msg
checkboxWithAttributes { namespace, labelText, onCheck, status, state, requiredText } attributes checked =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        requiredIndicator =
            requiredText
                |> Maybe.map (\required -> span [ class [ Required ], title required ] [ Html.text "*" ])
                |> Maybe.withDefault HtmlExtra.none
    in
    div [ class [ CheckBoxContainer ] ]
        [ Error.error { namespace = namespace } status
        , label [ class [ CheckBox Large ] ]
            [ input
                ([ type_ "checkbox"
                 , Html.Events.onCheck (onCheck { onlyStateChange = False } state)
                 , Html.Attributes.checked checked
                 , class [ CheckBoxInput ]
                 ]
                    ++ attributes
                )
                []
            , span [ class [ Label ] ] [ Html.text labelText, requiredIndicator ]
            ]
        ]


{-| Get a radio list view
-}
radioList :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> State -> String -> msg
    , status : Status
    , items : List { id : String, text : String }
    , requiredText : Maybe String
    }
    -> State
    -> String
    -> Html msg
radioList ({ namespace, labelText, onChange, status, items } as args) state selectedValue =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        namespacedId =
            Namespace.toString namespace ++ args.id

        stateData =
            unwrap state

        onValidationStateChange validationState =
            onChange { onlyStateChange = True } (State validationState) selectedValue
    in
    FormControl.groupFormControl
        { namespace = args.namespace
        , size = Large
        , id = args.id
        , labelText = args.labelText
        , helpText = args.helpText
        , status = args.status
        , onValidationStateChange = onValidationStateChange
        , requiredText = args.requiredText
        }
        stateData
        (div [ class [ RadioList Large ] ]
            (items
                |> List.map
                    (toRadio
                        { namespace = namespace
                        , labelText = labelText
                        , onChange = onChange { onlyStateChange = False } state
                        }
                        selectedValue
                    )
            )
        )


{-| Get a checkbox list view
-}
checkBoxList :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> State -> Set String -> msg
    , status : Status
    , items : List { id : String, text : String }
    , status : Status
    , requiredText : Maybe String
    }
    -> State
    -> Set String
    -> Html msg
checkBoxList ({ namespace, labelText, onChange, status, items } as args) state selectedValues =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        namespacedId =
            Namespace.toString namespace ++ args.id

        stateData =
            unwrap state

        onValidationStateChange validationState =
            onChange { onlyStateChange = True } (State validationState) selectedValues
    in
    FormControl.groupFormControl
        { namespace = args.namespace
        , size = Large
        , id = args.id
        , labelText = args.labelText
        , helpText = args.helpText
        , status = args.status
        , onValidationStateChange = onValidationStateChange
        , requiredText = args.requiredText
        }
        stateData
        (div [ class [ CheckBoxList Large ] ]
            (items
                |> List.map
                    (toCheckBox
                        { namespace = namespace
                        , labelText = labelText
                        , onChange = onChange { onlyStateChange = False } state
                        }
                        selectedValues
                    )
            )
        )


{-| The FileInfo type
-}
type alias FileInfo =
    { name : String, fileType : String, progressPercentage : Maybe Float }


{-| Get a file view
-}
file :
    { namespace : Namespace
    , id : String
    , labelText : String
    , browseText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> State -> FileInfo -> msg
    , status : Status
    , requiredText : Maybe String
    }
    -> State
    -> FileInfo
    -> Html msg
file args (State state) fileInfo =
    let
        { class } =
            args.namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        onChange =
            args.onChange { onlyStateChange = False } (State state)

        onValidationStateChange validationState =
            args.onChange { onlyStateChange = True } (State validationState) fileInfo
    in
    div [ class [ File ] ]
        [ FormControl.labelWrapped
            { namespace = args.namespace
            , size = Large
            , id = args.id
            , labelClass = []
            , labelText = args.labelText
            , helpText = args.helpText
            , status = args.status
            , onValidationStateChange = onValidationStateChange
            , requiredText = args.requiredText
            }
            state
            (div [ class [ FileButton Large ] ]
                [ input
                    [ id args.id
                    , type_ "file"
                    , on "change" (Json.Decode.map onChange fileEventDecoder)
                    ]
                    []
                , span [ class [ FileName, Input Large ], title fileInfo.name ] [ Html.text fileInfo.name ]
                , span [ tabindex 0, class [ BaseButton, Button Primary Large ] ] [ Html.text args.browseText ]
                , progressIndicator args fileInfo
                ]
            )
            HtmlExtra.none
        ]


{-| Get a progress indicator view
-}
progressIndicator : { a | namespace : Namespace } -> FileInfo -> Html msg
progressIndicator args fileInfo =
    fileInfo.progressPercentage
        |> Maybe.map (\value -> Loading.progress args { max = 100.0, value = value })
        |> Maybe.withDefault HtmlExtra.none


toCheckBox : { namespace : Namespace, onChange : Set String -> msg, labelText : String } -> Set String -> { id : String, text : String } -> Html msg
toCheckBox { namespace, onChange, labelText } selectedValues item =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        onClickHandler =
            if Set.member item.id selectedValues then
                Set.remove item.id selectedValues
                    |> onChange

            else
                Set.insert item.id selectedValues
                    |> onChange
    in
    label [ class [ CheckBoxContainer ] ]
        [ input
            [ type_ "checkbox"
            , name labelText
            , value item.id
            , class [ CheckBoxInput ]
            , onClick onClickHandler
            , checked (Set.member item.id selectedValues)
            ]
            []
        , span [ class [ RadioText ] ] [ Html.text item.text ]
        ]


toRadio : { namespace : Namespace, onChange : String -> msg, labelText : String } -> String -> { id : String, text : String } -> Html msg
toRadio { namespace, onChange, labelText } selectedValue item =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace
    in
    label [ class [ RadioContainer ] ]
        [ input
            [ type_ "radio"
            , name labelText
            , value item.id
            , class [ RadioInput ]
            , onClick (onChange item.id)
            , checked (selectedValue == item.id)
            ]
            []
        , span [ class [ RadioText ] ] [ Html.text item.text ]
        ]


unwrap : State -> Message.State
unwrap (State stateData) =
    stateData


fileEventDecoder : Json.Decode.Decoder FileInfo
fileEventDecoder =
    decode FileInfo
        |> requiredAt [ "target", "files", "0", "name" ] Json.Decode.string
        |> requiredAt [ "target", "files", "0", "type" ] Json.Decode.string
        |> Json.Decode.Pipeline.hardcoded Nothing
