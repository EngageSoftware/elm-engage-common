module Engage.Form.Field exposing
    ( checkbox
    , checkboxWithAttributes
    , dateField
    , datepickerField
    , dropdownField
    , dropdownFieldValueSort
    , dropdownFieldWithAttributes
    , fieldId
    , inputField
    , inputFieldWithAttributes
    , localizeHelp
    , localizeLabel
    , phoneField
    , radioListField
    , validate
    )

{-| Form.Field

@docs checkbox, checkboxWithAttributes, dateField, datepickerField, dropdownField, dropdownFieldValueSort, dropdownFieldWithAttributes, fieldId, inputField, inputFieldWithAttributes, localizeHelp, localizeLabel, phoneField, radioListField, validate

-}

import Date exposing (Date)
import Dict exposing (Dict)
import Engage.Entity.PhoneNumber exposing (PhoneNumber)
import Engage.Localization as Localization exposing (Localization)
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.UI.Datepicker as Datepicker
import Engage.UI.Dropdown as Dropdown
import Engage.UI.Error as Error
import Engage.UI.Input as Input
import Engage.Validation as Validation exposing (ValidationErrors)
import Html exposing (Html)


type alias InputFieldArgs field msg =
    { namespace : Namespace
    , field : field
    , onChange : ValidationErrors field -> { onlyStateChange : Bool } -> Input.State -> String -> msg
    , localization : Localization
    , required : Bool
    }


type alias PhoneFieldArgs field msg =
    { namespace : Namespace
    , isoCodeField : field
    , field : field
    , onChange : ValidationErrors field -> { onlyStateChange : Bool } -> Input.PhoneState -> PhoneNumber -> Cmd msg -> msg
    , localization : Localization
    , required : Bool
    }


{-| Get the phone field view
-}
phoneField : PhoneFieldArgs field msg -> ValidationErrors field -> Input.PhoneState -> PhoneNumber -> Html msg
phoneField args validations state phoneNumber =
    let
        dialCodeValidations updatedPhoneNumber validations =
            case args.required of
                True ->
                    validate args.isoCodeField updatedPhoneNumber.isoCode validations

                False ->
                    validations

        phoneNumberValidations updatedPhoneNumber validations =
            case args.required of
                True ->
                    validate args.field updatedPhoneNumber.phoneNumber validations

                False ->
                    validations

        onChange { onlyStateChange } updatedState updatedPhoneNumber =
            args.onChange
                (if onlyStateChange || phoneNumber == updatedPhoneNumber then
                    validations

                 else
                    validations
                        |> dialCodeValidations updatedPhoneNumber
                        |> phoneNumberValidations updatedPhoneNumber
                )
                { onlyStateChange = onlyStateChange }
                updatedState
                updatedPhoneNumber

        requiredText =
            if args.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" args)

            else
                Nothing
    in
    Input.phone
        { namespace = Namespace.engagecore
        , id = fieldId args.namespace args.field
        , labelText = localizeLabel args
        , helpText = localizeHelp args
        , requiredText = requiredText
        , onChange = onChange
        , status =
            Error.merge
                (Validation.fieldError args.localization args.field validations)
                (Validation.fieldError args.localization args.isoCodeField validations)
        }
        state
        phoneNumber


{-| Get the input field view
-}
inputField : InputFieldArgs field msg -> ValidationErrors field -> Input.State -> String -> Html msg
inputField args validations state value =
    inputFieldWithAttributes args validations [] state value


{-| Get the input field with attributes view
-}
inputFieldWithAttributes : InputFieldArgs field msg -> ValidationErrors field -> List (Html.Attribute msg) -> Input.State -> String -> Html msg
inputFieldWithAttributes args validations attributes state value =
    let
        updatedValidations updatedValue =
            case args.required of
                True ->
                    validate args.field updatedValue validations

                False ->
                    validations

        onChange { onlyStateChange } updatedState updatedValue =
            args.onChange
                (if onlyStateChange then
                    validations

                 else
                    updatedValidations updatedValue
                )
                { onlyStateChange = onlyStateChange }
                updatedState
                updatedValue

        requiredText =
            if args.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" args)

            else
                Nothing
    in
    Input.textWithAttributes
        { namespace = Namespace.engagecore
        , id = fieldId args.namespace args.field
        , labelText = localizeLabel args
        , helpText = localizeHelp args
        , requiredText = requiredText
        , onChange = onChange
        , status = Validation.fieldError args.localization args.field validations
        }
        attributes
        state
        value


type alias RadioListFieldArgs field msg =
    { namespace : Namespace
    , field : field
    , onChange : ValidationErrors field -> { onlyStateChange : Bool } -> Input.State -> String -> msg
    , localization : Localization
    , required : Bool
    , items : List { id : String, text : String }
    }


{-| Get the radio field view
-}
radioListField : RadioListFieldArgs field msg -> ValidationErrors field -> Input.State -> String -> Html msg
radioListField args validations state value =
    let
        updatedValidations updatedValue =
            case args.required of
                True ->
                    validate args.field updatedValue validations

                False ->
                    validations

        onChange { onlyStateChange } updatedState updatedValue =
            args.onChange
                (if onlyStateChange then
                    validations

                 else
                    updatedValidations updatedValue
                )
                { onlyStateChange = onlyStateChange }
                updatedState
                updatedValue

        requiredText =
            if args.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" args)

            else
                Nothing
    in
    Input.radioList
        { namespace = Namespace.engagecore
        , id = fieldId args.namespace args.field
        , labelText = localizeLabel args
        , helpText = localizeHelp args
        , requiredText = requiredText
        , onChange = onChange
        , status = Validation.fieldError args.localization args.field validations
        , items = args.items
        }
        state
        value


type alias DropdownFieldArgs field msg =
    { namespace : Namespace
    , onChange : ValidationErrors field -> Dropdown.State -> Maybe ( String, String ) -> msg
    , localization : Localization
    , field : field
    , required : Bool
    , items : Dict String Dropdown.Item
    }


{-| Get the dropdown field view
-}
dropdownField : DropdownFieldArgs field msg -> ValidationErrors field -> Dropdown.State -> Maybe String -> Html msg
dropdownField args validations state value =
    dropdownFieldWithAttributes args validations [] state value


{-| Get the dropdown field value sort view
-}
dropdownFieldValueSort : DropdownFieldArgs field msg -> ValidationErrors field -> Dropdown.State -> Maybe String -> Bool -> Html msg
dropdownFieldValueSort args validations state value reverseSort =
    dropdownFieldWithAttributesValueSort args validations [] state value reverseSort


{-| Get the dropdown field with attributes value sort view
-}
dropdownFieldWithAttributesValueSort : DropdownFieldArgs field msg -> ValidationErrors field -> List (Html.Attribute msg) -> Dropdown.State -> Maybe String -> Bool -> Html msg
dropdownFieldWithAttributesValueSort args validations attributes state value reverseSort =
    let
        required =
            args.required && not (Dict.isEmpty args.items)

        updatedValidations updatedValue =
            if required then
                validateMaybe args.field updatedValue validations

            else
                validations

        toKeyValue updatedValue =
            updatedValue
                |> Maybe.andThen (\key -> Dict.get key args.items)
                |> Maybe.map2 (\key item -> ( key, item.text )) updatedValue

        onChange { onlyStateChange } state updatedValue =
            args.onChange (updatedValidations updatedValue) state (toKeyValue updatedValue)

        requiredText =
            if required then
                Just (Localization.localizeStringWithDefault "Required" "Required" args)

            else
                Nothing
    in
    Dropdown.dropdownWithAttributes
        { id = fieldId args.namespace args.field
        , labelText = localizeLabel args
        , requiredText = requiredText
        , items =
            args.items
                |> Dict.values
                |> List.map (\item -> { item | value = String.toInt item.value |> Result.withDefault 0 })
                |> List.sortBy .value
                |> List.map (\item -> { item | value = toString item.value })
                |> (if reverseSort then
                        List.reverse

                    else
                        \items -> items
                   )
        , onChange = onChange
        , status = Validation.fieldError args.localization args.field validations
        , namespace = Namespace.engagecore
        }
        attributes
        state
        value


{-| Get the dropdown field with attributes view
-}
dropdownFieldWithAttributes : DropdownFieldArgs field msg -> ValidationErrors field -> List (Html.Attribute msg) -> Dropdown.State -> Maybe String -> Html msg
dropdownFieldWithAttributes args validations attributes state value =
    let
        required =
            args.required && not (Dict.isEmpty args.items)

        updatedValidations updatedValue =
            if required then
                validateMaybe args.field updatedValue validations

            else
                validations

        toKeyValue updatedValue =
            updatedValue
                |> Maybe.andThen (\key -> Dict.get key args.items)
                |> Maybe.map2 (\key item -> ( key, item.text )) updatedValue

        onChange { onlyStateChange } state updatedValue =
            args.onChange (updatedValidations updatedValue) state (toKeyValue updatedValue)

        requiredText =
            if required then
                Just (Localization.localizeStringWithDefault "Required" "Required" args)

            else
                Nothing
    in
    Dropdown.dropdownWithAttributes
        { id = fieldId args.namespace args.field
        , labelText = localizeLabel args
        , requiredText = requiredText
        , items = args.items |> Dict.values |> List.sortBy .text
        , onChange = onChange
        , status = Validation.fieldError args.localization args.field validations
        , namespace = Namespace.engagecore
        }
        attributes
        state
        value


type alias DatepickerFieldArgs field msg =
    { namespace : Namespace
    , onChange : ValidationErrors field -> Datepicker.State -> Maybe Date -> msg
    , onStateChange : ValidationErrors field -> Datepicker.State -> msg
    , localization : Localization
    , field : field
    , required : Bool
    , now : Date
    }


{-| Get the datepicker field view
-}
datepickerField : DatepickerFieldArgs field msg -> ValidationErrors field -> Datepicker.State -> Maybe Date -> Html msg
datepickerField args validations state value =
    let
        updatedValidations updatedValue =
            if args.required then
                validateMaybe args.field updatedValue validations

            else
                validations

        onStateChange state =
            args.onStateChange (updatedValidations value) state

        onChange state updatedValue =
            args.onChange (updatedValidations updatedValue) state updatedValue

        requiredText =
            if args.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" args)

            else
                Nothing
    in
    Datepicker.datepicker
        { id = fieldId args.namespace args.field
        , onChange = onChange
        , onStateChange = onStateChange
        , labelText = localizeLabel args
        , requiredText = requiredText
        , namespace = Namespace.engagecore
        , status = Validation.fieldError args.localization args.field validations
        }
        state
        value


{-| Get the date field view
-}
dateField : DatepickerFieldArgs field msg -> ValidationErrors field -> Datepicker.State -> Maybe Date -> Html msg
dateField args validations state value =
    let
        updatedValidations updatedValue =
            if args.required then
                validateMaybe args.field updatedValue validations

            else
                validations

        onStateChange state =
            args.onStateChange (updatedValidations value) state

        onChange state updatedValue =
            args.onChange (updatedValidations updatedValue) state updatedValue

        requiredText =
            if args.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" args)

            else
                Nothing
    in
    Datepicker.date
        { id = fieldId args.namespace args.field
        , onChange = onChange
        , onStateChange = onStateChange
        , labelText = localizeLabel args
        , requiredText = requiredText
        , namespace = Namespace.engagecore
        , status = Validation.fieldError args.localization args.field validations
        }
        state
        value


type alias CheckboxFieldArgs field msg =
    { namespace : Namespace
    , field : field
    , onCheck : ValidationErrors field -> { onlyStateChange : Bool } -> Input.State -> Bool -> msg
    , localization : Localization
    , required : Bool
    }


{-| Get the checkbox view
-}
checkbox : CheckboxFieldArgs field msg -> ValidationErrors field -> Input.State -> Bool -> Html msg
checkbox args validations state value =
    checkboxWithAttributes args validations [] state value


{-| Get the checkbox with attributes view
-}
checkboxWithAttributes : CheckboxFieldArgs field msg -> ValidationErrors field -> List (Html.Attribute msg) -> Input.State -> Bool -> Html msg
checkboxWithAttributes args validations attributes state value =
    let
        updatedValidations updatedValue =
            case args.required of
                True ->
                    validateBool args.field updatedValue validations

                False ->
                    validations

        onCheck { onlyStateChange } updatedState updatedValue =
            args.onCheck
                (if onlyStateChange then
                    validations

                 else
                    updatedValidations updatedValue
                )
                { onlyStateChange = onlyStateChange }
                updatedState
                updatedValue

        requiredText =
            if args.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" args)

            else
                Nothing
    in
    Input.checkboxWithAttributes
        { namespace = Namespace.engagecore
        , labelText = localizeLabel args
        , helpText = localizeHelp args
        , requiredText = requiredText
        , onCheck = onCheck
        , status = Validation.fieldError args.localization args.field validations
        , state = state
        }
        attributes
        value



-- HELPERS


{-| Get the fieldId
-}
fieldId : Namespace -> field -> String
fieldId namespace field =
    Namespace.toString namespace ++ toString field


validateBool : field -> Bool -> ValidationErrors field -> ValidationErrors field
validateBool field value validations =
    let
        cleanValidations =
            validations |> Validation.filter [ field ]
    in
    cleanValidations
        ++ Validation.validateBoolField (Validation.localize field) field (always value) ()


{-| Validate a field
-}
validate : field -> String -> ValidationErrors field -> ValidationErrors field
validate field value validations =
    let
        cleanValidations =
            validations |> Validation.filter [ field ]
    in
    cleanValidations
        ++ Validation.validateStringField (Validation.localize field) field (always value) ()


validateMaybe : field -> Maybe a -> ValidationErrors field -> ValidationErrors field
validateMaybe field value validations =
    let
        cleanValidations =
            validations |> Validation.filter [ field ]
    in
    cleanValidations
        ++ Validation.validateMaybeField (Validation.localize field) field (always value) ()


{-| Localize a label String
-}
localizeLabel : { a | field : field, localization : Localization } -> String
localizeLabel ({ field } as args) =
    Localization.localizeString (toString field ++ ".label") args


{-| Localize a help String
-}
localizeHelp : { a | field : field, localization : Localization } -> String
localizeHelp ({ field } as args) =
    Localization.localizeStringWithDefault "" (toString field ++ ".help") args
