module Engage.Form.Field exposing
    ( textField, textFieldWithAttributes
    , passwordField, passwordFieldWithAttributes
    , checkbox, checkboxWithAttributes
    , dateInputField
    , dropdownField, dropdownFieldValueSort, dropdownFieldWithAttributes
    , phoneField
    , radioListField
    , localizeHelp, localizeLabel, localizeInvalid
    , validate, fieldId
    )

{-| Form.Field

Form fields with validation


## Text

@docs textField, textFieldWithAttributes


## Password

@docs passwordField, passwordFieldWithAttributes


## Checkbox

@docs checkbox, checkboxWithAttributes


## Date

@docs dateInputField


## Dropdown

@docs dropdownField, dropdownFieldValueSort, dropdownFieldWithAttributes


## Phone number

@docs phoneField


## Radio list

@docs radioListField


## Localization helpers

@docs localizeHelp, localizeLabel, localizeInvalid


## Validation helpers

@docs validate, fieldId

-}

import Date exposing (Date)
import Dict exposing (Dict)
import Engage.Entity.PhoneNumber exposing (PhoneNumber)
import Engage.Localization as Localization exposing (Localization)
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.UI.Dropdown as Dropdown
import Engage.UI.Error as Error
import Engage.UI.Input as Input
import Engage.Validation as Validation exposing (ValidationErrors)
import Html exposing (Html)


type InputField field msg
    = TextField (InputFieldArgs field msg {})
    | PasswordField (InputFieldArgs field msg (PasswordFieldArgs field msg))


type alias InputFieldArgs field msg args =
    { args
        | namespace : Namespace
        , field : field
        , fieldKey : String
        , onChange : ValidationErrors field -> { onlyStateChange : Bool } -> Input.State -> String -> msg
        , localization : Localization
        , required : Bool
    }


inputFieldArgs : InputField field msg -> InputFieldArgs field msg {}
inputFieldArgs inputField =
    case inputField of
        TextField { namespace, field, fieldKey, onChange, localization, required } ->
            { namespace = namespace, field = field, fieldKey = fieldKey, onChange = onChange, localization = localization, required = required }

        PasswordField { namespace, field, fieldKey, onChange, localization, required } ->
            { namespace = namespace, field = field, fieldKey = fieldKey, onChange = onChange, localization = localization, required = required }


type alias PasswordFieldArgs field msg =
    { strengthMeter : Maybe (List String)
    , hasFocus : Maybe (ValidationErrors field -> Bool -> msg)
    }


type alias PhoneFieldArgs field msg =
    { namespace : Namespace
    , isoCodeField : field
    , isoCodeFieldKey : String
    , field : field
    , fieldKey : String
    , onChange : ValidationErrors field -> { onlyStateChange : Bool } -> Input.PhoneState -> PhoneNumber -> Cmd msg -> msg
    , localization : Localization
    , required : Bool
    }


{-| Get the phone field view
-}
phoneField : PhoneFieldArgs field msg -> ValidationErrors field -> Input.PhoneState -> PhoneNumber -> Html msg
phoneField args validations state phoneNumber =
    let
        dialCodeValidations updatedPhoneNumber newValidations =
            if args.required then
                validate args.isoCodeField args.isoCodeFieldKey updatedPhoneNumber.isoCode newValidations

            else
                newValidations

        phoneNumberValidations updatedPhoneNumber newValidations =
            if args.required then
                validate args.field args.fieldKey updatedPhoneNumber.phoneNumber newValidations

            else
                newValidations

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
        , id = fieldId args.namespace args.fieldKey
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


{-| Get the text field view
-}
textField : InputFieldArgs field msg {} -> ValidationErrors field -> Input.State -> String -> Html msg
textField args validations state value =
    textFieldWithAttributes args validations [] state value


{-| Get the text field with attributes view
-}
textFieldWithAttributes : InputFieldArgs field msg {} -> ValidationErrors field -> List (Html.Attribute msg) -> Input.State -> String -> Html msg
textFieldWithAttributes args validations attributes state value =
    inputFieldWithAttributes (TextField args) validations attributes state value


{-| Get the password field view
-}
passwordField : InputFieldArgs field msg (PasswordFieldArgs field msg) -> ValidationErrors field -> Input.State -> String -> Html msg
passwordField args validations state value =
    passwordFieldWithAttributes args validations [] state value


{-| Get the password field with attributes view
-}
passwordFieldWithAttributes : InputFieldArgs field msg (PasswordFieldArgs field msg) -> ValidationErrors field -> List (Html.Attribute msg) -> Input.State -> String -> Html msg
passwordFieldWithAttributes args validations attributes state value =
    inputFieldWithAttributes (PasswordField args) validations attributes state value


inputFieldWithAttributes : InputField field msg -> ValidationErrors field -> List (Html.Attribute msg) -> Input.State -> String -> Html msg
inputFieldWithAttributes inputField validations attributes state value =
    let
        args =
            inputFieldArgs inputField

        updatedValidations updatedValue =
            if args.required then
                validate args.field args.fieldKey updatedValue validations

            else
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
    case inputField of
        PasswordField passwordArgs ->
            let
                hasFocus =
                    passwordArgs.hasFocus |> Maybe.map (\focus -> focus validations)
            in
            Input.passwordWithAttributes
                { namespace = Namespace.engagecore
                , id = fieldId args.namespace args.fieldKey
                , labelText = localizeLabel args
                , helpText = localizeHelp args
                , requiredText = requiredText
                , onChange = onChange
                , status = Validation.fieldError args.localization args.field validations
                , strengthMeter = passwordArgs.strengthMeter
                , hasFocus = hasFocus
                }
                attributes
                state
                value

        TextField _ ->
            Input.textWithAttributes
                { namespace = Namespace.engagecore
                , id = fieldId args.namespace args.fieldKey
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
    , fieldKey : String
    , onChange : ValidationErrors field -> { onlyStateChange : Bool } -> Input.State -> String -> msg
    , localization : Localization
    , required : Bool
    , items :
        List
            { id : String
            , text : String
            }
    }


{-| Get the radio field view
-}
radioListField : RadioListFieldArgs field msg -> ValidationErrors field -> Input.State -> String -> Html msg
radioListField args validations state value =
    let
        updatedValidations updatedValue =
            if args.required then
                validate args.field args.fieldKey updatedValue validations

            else
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
        , id = fieldId args.namespace args.fieldKey
        , labelText = localizeLabel args
        , helpText = localizeHelp args
        , requiredText = requiredText
        , onChange = onChange
        , status = Validation.fieldError args.localization args.field validations
        , items = List.map (\item -> { id = item.id, content = Html.text item.text }) args.items
        }
        state
        value


type alias DropdownFieldArgs field msg =
    { namespace : Namespace
    , onChange : ValidationErrors field -> Dropdown.State -> Maybe ( String, String ) -> msg
    , localization : Localization
    , field : field
    , fieldKey : String
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
                validateMaybe args.field args.fieldKey updatedValue validations

            else
                validations

        toKeyValue updatedValue =
            updatedValue
                |> Maybe.andThen (\key -> Dict.get key args.items)
                |> Maybe.map2 (\key item -> ( key, item.text )) updatedValue

        onChange { onlyStateChange } updatedState updatedValue =
            args.onChange
                (if onlyStateChange then
                    validations

                 else
                    updatedValidations updatedValue
                )
                updatedState
                (toKeyValue updatedValue)

        requiredText =
            if required then
                Just (Localization.localizeStringWithDefault "Required" "Required" args)

            else
                Nothing
    in
    Dropdown.dropdownWithAttributes
        { id = fieldId args.namespace args.fieldKey
        , labelText = localizeLabel args
        , requiredText = requiredText
        , items =
            args.items
                |> Dict.values
                |> List.sortBy .value
                |> (if reverseSort then
                        List.reverse

                    else
                        \items -> items
                   )
        , onChange = onChange
        , status = Validation.fieldError args.localization args.field validations
        , namespace = Namespace.engagecore
        , withEmptyItem = True
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
                validateMaybe args.field args.fieldKey updatedValue validations

            else
                validations

        toKeyValue updatedValue =
            updatedValue
                |> Maybe.andThen (\key -> Dict.get key args.items)
                |> Maybe.map2 (\key item -> ( key, item.text )) updatedValue

        onChange { onlyStateChange } updatedState updatedValue =
            args.onChange
                (if onlyStateChange then
                    validations

                 else
                    updatedValidations updatedValue
                )
                updatedState
                (toKeyValue updatedValue)

        requiredText =
            if required then
                Just (Localization.localizeStringWithDefault "Required" "Required" args)

            else
                Nothing
    in
    Dropdown.dropdownWithAttributes
        { id = fieldId args.namespace args.fieldKey
        , labelText = localizeLabel args
        , requiredText = requiredText
        , items = args.items |> Dict.values |> List.sortBy .text
        , onChange = onChange
        , status = Validation.fieldError args.localization args.field validations
        , namespace = Namespace.engagecore
        , withEmptyItem = True
        }
        attributes
        state
        value


type alias DateInputFieldArgs field msg =
    { namespace : Namespace
    , onChange : ValidationErrors field -> Input.State -> Maybe Date -> msg
    , localization : Localization
    , field : field
    , fieldKey : String
    , required : Bool
    }


{-| Get the date input field view
-}
dateInputField : DateInputFieldArgs field msg -> ValidationErrors field -> Input.State -> Maybe Date -> Html msg
dateInputField args validations state value =
    let
        updatedValidations updatedValue =
            Validation.validateField [ Validation.validateMaybeField (localizeInvalid args) args.field (always updatedValue) ] ()

        noValidationErrors newValue =
            List.isEmpty (updatedValidations newValue)

        onChange updatedState newDate =
            args.onChange
                (if noValidationErrors newDate then
                    updatedValidations newDate

                 else
                    validations
                )
                updatedState
                newDate

        onFocusChange hasFocus =
            if hasFocus then
                args.onChange validations state value

            else
                args.onChange (updatedValidations value) state value

        requiredText =
            if args.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" args)

            else
                Nothing
    in
    Input.date
        { id = fieldId args.namespace args.fieldKey
        , onChange = onChange
        , onFocusChange = Just onFocusChange
        , labelText = localizeLabel args
        , helpText = localizeHelp args
        , requiredText = requiredText
        , namespace = Namespace.engagecore
        , status = Validation.fieldError args.localization args.field validations
        }
        state
        value


type alias CheckboxFieldArgs field msg =
    { namespace : Namespace
    , field : field
    , fieldKey : String
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
            if args.required then
                validateBool args.field args.fieldKey updatedValue validations

            else
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
fieldId : Namespace -> String -> String
fieldId namespace field =
    Namespace.toString namespace ++ field


validateBool : field -> String -> Bool -> ValidationErrors field -> ValidationErrors field
validateBool field fieldKey value validations =
    let
        cleanValidations =
            validations |> Validation.filter [ field ]
    in
    cleanValidations
        ++ Validation.validateField [ Validation.validateBoolField (Validation.localizeRequired fieldKey) field (always value) ] ()


{-| Validate a field
-}
validate : field -> String -> String -> ValidationErrors field -> ValidationErrors field
validate field fieldKey value validations =
    let
        cleanValidations =
            validations |> Validation.filter [ field ]
    in
    cleanValidations
        ++ Validation.validateField [ Validation.validateStringField (Validation.localizeRequired fieldKey) field (always value) ] ()


validateMaybe : field -> String -> Maybe a -> ValidationErrors field -> ValidationErrors field
validateMaybe field fieldKey value validations =
    let
        cleanValidations =
            validations |> Validation.filter [ field ]
    in
    cleanValidations
        ++ Validation.validateField [ Validation.validateMaybeField (Validation.localizeRequired fieldKey) field (always value) ] ()


{-| Localize a label String
-}
localizeLabel : { a | fieldKey : String, localization : Localization } -> String
localizeLabel ({ fieldKey } as args) =
    Localization.localizeString (fieldKey ++ ".label") args


{-| Localize a help String
-}
localizeHelp : { a | fieldKey : String, localization : Localization } -> String
localizeHelp ({ fieldKey } as args) =
    Localization.localizeStringWithDefault "" (fieldKey ++ ".help") args


{-| Localize a invalid String
-}
localizeInvalid : { a | fieldKey : String, localization : Localization } -> String
localizeInvalid ({ fieldKey } as args) =
    Localization.localizeStringWithDefault "" (fieldKey ++ ".invalid") args
