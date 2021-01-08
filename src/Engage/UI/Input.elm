module Engage.UI.Input exposing
    ( text, textArea, textWithAttributes, textWithSize
    , password, passwordWithAttributes
    , number, smallNumber, bigNumber
    , checkBoxList, checkbox, checkboxWithAttributes
    , file, FileInfo
    , phone, PhoneState, initialPhoneState
    , radioList, reset, date
    , State, initialState
    )

{-| UI.Input


## Text

@docs text, textArea, textWithAttributes, textWithSize


## Password

@docs password, passwordWithAttributes


## Numbers

@docs number, smallNumber, bigNumber


## Checkboxes

@docs checkBoxList, checkbox, checkboxWithAttributes


## File

@docs file, FileInfo


## Phone

@docs phone, PhoneState, initialPhoneState


## Misc

@docs radioList, reset, date


## State

@docs State, initialState

-}

import Date exposing (Date)
import Engage.CssHelpers
import Engage.Entity.PhoneNumber exposing (PhoneNumber)
import Engage.Html.Extra as HtmlExtra
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.String
import Engage.Styles.Class exposing (Class(..), Importance(..), Size(..), getSizeString)
import Engage.UI.Error as Error exposing (Status)
import Engage.UI.FormControl as FormControl
import Engage.UI.Loading as Loading
import Engage.UI.Message as Message
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Styled.Attributes exposing (fromUnstyled)
import Input.BigNumber
import Input.Number
import Input.Text
import IntlPhoneInput
import IntlPhoneInput.Config
import Json.Decode exposing (succeed)
import Json.Decode.Pipeline exposing (requiredAt)
import List.Extra
import Maybe.Extra
import Set exposing (Set)
import Zxcvbn exposing (ZxcvbnResult)


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
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        safeId =
            Engage.String.toSafeId id

        defaultConfig =
            IntlPhoneInput.Config.config
                safeId
                (\phoneState phoneNumberValue cmd ->
                    onChange { onlyStateChange = False } { state | phoneInput = phoneState } phoneNumberValue cmd
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
            (List.map fromUnstyled (class [ "Input-" ++ getSizeString size ] :: attributes))
            config
            state.phoneInput
            phoneNumber
        )


{-| Get the date input view
-}
date :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : State -> Maybe Date -> msg
    , onFocusChange : Maybe (Bool -> msg)
    , status : Status
    , requiredText : Maybe String
    }
    -> State
    -> Maybe Date
    -> Html msg
date { namespace, id, labelText, helpText, onChange, status, requiredText, onFocusChange } state dateValue =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        safeId =
            Engage.String.toSafeId id

        onFocusAttribute =
            onFocusChange
                |> Maybe.map (\f -> f True)
                |> Maybe.map onFocus
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        onBlurAttribute =
            onFocusChange
                |> Maybe.map (\f -> f False)
                |> Maybe.map onBlur
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        stateData =
            unwrap state

        onValidationStateChange validationState =
            onChange (State validationState) dateValue
    in
    FormControl.formControl
        { namespace = namespace
        , size = Large
        , id = safeId
        , labelText = labelText
        , helpText = helpText
        , status = status
        , onValidationStateChange = onValidationStateChange
        , requiredText = requiredText
        }
        stateData
        (Html.input
            ([ Html.Attributes.id id
             , class [ "Input-Large" ]
             , Html.Attributes.attribute "type" "date"
             , Html.Attributes.value (dateValue |> Maybe.map Date.toIsoString |> Maybe.withDefault "")
             , Html.Events.onInput (\dateString -> onChange state (dateString |> Date.fromIsoString |> Result.toMaybe))
             ]
                |> List.append onFocusAttribute
                |> List.append onBlurAttribute
            )
            []
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
    inputWithSizeAndAttributes
        { id = id
        , labelText = labelText
        , helpText = helpText
        , onChange = onChange
        , status = status
        , size = Large
        , namespace = namespace
        , requiredText = requiredText
        , inputType = Nothing
        , hasFocus = Nothing
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
textWithSize { namespace, id, labelText, helpText, onChange, status, size, requiredText } state value =
    inputWithSizeAndAttributes
        { id = id
        , labelText = labelText
        , helpText = helpText
        , onChange = onChange
        , status = status
        , size = size
        , namespace = namespace
        , requiredText = requiredText
        , inputType = Nothing
        , hasFocus = Nothing
        }
        []
        state
        value


{-| Get a text with size and attributes view
-}
inputWithSizeAndAttributes :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> State -> String -> msg
    , status : Status
    , size : Size
    , requiredText : Maybe String
    , inputType : Maybe String
    , hasFocus : Maybe (Bool -> msg)
    }
    -> List (Html.Attribute msg)
    -> State
    -> String
    -> Html msg
inputWithSizeAndAttributes { namespace, id, labelText, helpText, onChange, status, size, requiredText, inputType, hasFocus } attributes state value =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

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
            { options | type_ = inputType |> Maybe.withDefault "text", hasFocus = hasFocus }
            (attributes ++ [ Html.Attributes.id id, class [ "Input-" ++ getSizeString size ] ])
            value
        )


{-| Get a password input
-}
password :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> State -> String -> msg
    , status : Status
    , requiredText : Maybe String
    , strengthMeter : Maybe (List String)
    , hasFocus : Maybe (Bool -> msg)
    }
    -> State
    -> String
    -> Html msg
password args state value =
    passwordWithAttributes args [] state value


{-| Get a password with attributes input
-}
passwordWithAttributes :
    { namespace : Namespace
    , id : String
    , labelText : String
    , helpText : String
    , onChange : { onlyStateChange : Bool } -> State -> String -> msg
    , status : Status
    , requiredText : Maybe String
    , strengthMeter : Maybe (List String)
    , hasFocus : Maybe (Bool -> msg)
    }
    -> List (Html.Attribute msg)
    -> State
    -> String
    -> Html msg
passwordWithAttributes { namespace, id, labelText, helpText, onChange, status, requiredText, strengthMeter, hasFocus } attributes state value =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        ( passwordScore, passwordFeedback ) =
            case strengthMeter of
                Just extraDict ->
                    let
                        { score, feedback } =
                            Zxcvbn.zxcvbn extraDict value
                    in
                    ( score, feedback )

                Nothing ->
                    ( -1, { warning = "", suggestions = [] } )
    in
    div []
        (inputWithSizeAndAttributes
            { id = id
            , labelText = labelText
            , helpText = helpText
            , onChange = onChange
            , status = status
            , size = Large
            , namespace = namespace
            , requiredText = requiredText
            , inputType = Just "password"
            , hasFocus = hasFocus
            }
            attributes
            state
            value
            :: (if Maybe.Extra.isJust strengthMeter && (String.isEmpty value |> not) then
                    [ Html.meter
                        [ Html.Attributes.max "4"
                        , Html.Attributes.value (String.fromInt passwordScore)
                        , class [ "PasswordFeedback-Meter" ]
                        ]
                        []
                    , Html.span [ class [ "PasswordFeedback-Warning" ] ] [ Html.text passwordFeedback.warning ]
                    ]

                else
                    []
               )
        )


{-| Get a number input
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
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

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
            [ Html.Attributes.id id, class [ "Input-" ++ getSizeString size ] ]
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
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

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
            [ Html.Attributes.id id, class [ "Input-Large" ] ]
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
        class =
            args.namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

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
            , class [ "TextArea-" ++ getSizeString args.size ]
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
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        requiredIndicator =
            requiredText
                |> Maybe.map (\required -> span [ class [ "Required" ], title required ] [ Html.text "*" ])
                |> Maybe.withDefault HtmlExtra.none
    in
    div [ class [ "CheckBoxContainer" ] ]
        [ Error.error { namespace = namespace } status
        , label [ class [ "CheckBox-Large" ] ]
            [ input
                ([ type_ "checkbox"
                 , Html.Events.onCheck (onCheck { onlyStateChange = False } state)
                 , Html.Attributes.checked checked
                 , class [ "CheckBoxInput" ]
                 ]
                    ++ attributes
                )
                []
            , span [ class [ "Label" ] ] [ Html.text labelText, requiredIndicator ]
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
    , items : List { id : String, content : Html msg }
    , requiredText : Maybe String
    }
    -> State
    -> String
    -> Html msg
radioList ({ namespace, labelText, onChange, items } as args) state selectedValue =
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
        (div [ class [ "RadioList-Large" ] ]
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
    , items : List { id : String, content : Html msg }
    , requiredText : Maybe String
    }
    -> State
    -> Set String
    -> Html msg
checkBoxList ({ namespace, labelText, onChange, items } as args) state selectedValues =
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
        (div [ class [ "CheckBoxList-Large" ] ]
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
    { name : String
    , fileType : String
    , progressPercentage : Maybe Float
    }


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
        class =
            args.namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        onChange =
            args.onChange { onlyStateChange = False } (State state)

        onValidationStateChange validationState =
            args.onChange { onlyStateChange = True } (State validationState) fileInfo
    in
    div [ class [ "File" ] ]
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
            (div [ class [ "FileButton-Large" ] ]
                [ input
                    [ id args.id
                    , type_ "file"
                    , on "change" (Json.Decode.map onChange fileEventDecoder)
                    ]
                    []
                , span [ class [ "FileName", "Input-Large" ], title fileInfo.name ] [ Html.text fileInfo.name ]
                , span [ tabindex 0, class [ "BaseButton", "Button-Primary-Large" ] ] [ Html.text args.browseText ]
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


toCheckBox : { namespace : Namespace, onChange : Set String -> msg, labelText : String } -> Set String -> { id : String, content : Html msg } -> Html msg
toCheckBox { namespace, onChange, labelText } selectedValues item =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        onClickHandler =
            if Set.member item.id selectedValues then
                Set.remove item.id selectedValues
                    |> onChange

            else
                Set.insert item.id selectedValues
                    |> onChange
    in
    label [ class [ "CheckBoxContainer" ] ]
        [ input
            [ type_ "checkbox"
            , name labelText
            , value item.id
            , class [ "CheckBoxInput" ]
            , onClick onClickHandler
            , checked (Set.member item.id selectedValues)
            ]
            []
        , span [ class [ "RadioText" ] ] [ item.content ]
        ]


toRadio : { namespace : Namespace, onChange : String -> msg, labelText : String } -> String -> { id : String, content : Html msg } -> Html msg
toRadio { namespace, onChange, labelText } selectedValue item =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    label [ class [ "RadioContainer" ] ]
        [ input
            [ type_ "radio"
            , name labelText
            , value item.id
            , class [ "RadioInput" ]
            , onClick (onChange item.id)
            , checked (selectedValue == item.id)
            ]
            []
        , span [ class [ "RadioText" ] ] [ item.content ]
        ]


unwrap : State -> Message.State
unwrap (State stateData) =
    stateData


fileEventDecoder : Json.Decode.Decoder FileInfo
fileEventDecoder =
    succeed FileInfo
        |> requiredAt [ "target", "files", "0", "name" ] Json.Decode.string
        |> requiredAt [ "target", "files", "0", "type" ] Json.Decode.string
        |> Json.Decode.Pipeline.hardcoded Nothing
