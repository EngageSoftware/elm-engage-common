module Engage.Custom.Field.Validation exposing (errorMessage, validateAllFieldGroup, validateCheckBox, validateCheckBoxList, validateDate, validateDropDown, validateEmail, validateField, validateFieldGroup, validateFile, validateNumber, validatePhone, validateQuantity, validateRadioList, validateText, validateUSState, validateZipCode)

import Dict
import Engage.Custom.Field.Helpers as Helpers
import Engage.Custom.Types exposing (..)
import Engage.Validation as Validation exposing (ValidationResult)
import Set exposing (Set)
import String


validateFieldGroup : { a | fieldId : Int } -> FieldGroup -> ValidationResult { fieldId : Int }
validateFieldGroup fieldId fieldSet =
    fieldSet.fields
        |> Dict.values
        |> List.concatMap (validateField fieldId)


validateAllFieldGroup : FieldGroup -> ValidationResult { fieldId : Int }
validateAllFieldGroup fieldSet =
    fieldSet.fields
        |> Dict.values
        |> List.concatMap (\fieldData -> validateField { fieldId = fieldData.fieldId } fieldData)


validateField : { a | fieldId : Int } -> Field -> ValidationResult { fieldId : Int }
validateField { fieldId } field =
    if field.fieldId == fieldId then
        case field.fieldType of
            TextBox _ ->
                validateText field

            LargeTextBox _ ->
                validateText field

            TextArea _ ->
                validateText field

            File _ ->
                validateFile field

            CheckBox _ ->
                validateCheckBox field

            DropDown _ ->
                validateDropDown field

            CheckBoxList _ ->
                validateCheckBoxList field

            RadioList _ ->
                validateRadioList field

            Quantity _ ->
                validateQuantity field

            Date _ ->
                validateDate field

            Email ->
                validateEmail field

            Phone ->
                validatePhone field

            ZipCode ->
                validateZipCode field

            USState ->
                validateUSState field

            Region _ ->
                validateDropDown field

            Country _ ->
                validateDropDown field

            Text ->
                []

            StaticForm staticFormType ->
                []

    else
        []


validateCheckBoxList : Field -> ValidationResult { fieldId : Int }
validateCheckBoxList field =
    case field.required of
        True ->
            let
                values =
                    Helpers.getValues field
                        |> Maybe.map Set.toList
                        |> Maybe.withDefault []
            in
            Validation.validateListNotEmptyField (errorMessage field) { fieldId = field.fieldId } (always values) ()

        False ->
            []


validateText : Field -> ValidationResult { fieldId : Int }
validateText field =
    case field.required of
        True ->
            let
                value =
                    Helpers.getValue field
                        |> Maybe.andThen List.head
            in
            Validation.validateMaybeStringField (errorMessage field) { fieldId = field.fieldId } (always value) ()

        False ->
            []


errorMessage : Field -> String
errorMessage field =
    if String.isEmpty field.errorMessage then
        field.label ++ ".Required"

    else
        field.errorMessage


validateFile : Field -> ValidationResult { fieldId : Int }
validateFile field =
    case field.required of
        True ->
            let
                fileInfo =
                    Helpers.getFileInfo field
                        |> Maybe.map .name
            in
            Validation.validateMaybeStringField field.errorMessage
                { fieldId = field.fieldId }
                (always fileInfo)
                ()

        False ->
            []


validateCheckBox : Field -> ValidationResult { fieldId : Int }
validateCheckBox field =
    case field.required of
        True ->
            let
                value =
                    Helpers.getBoolValue field |> Maybe.withDefault False
            in
            Validation.validateBoolField field.errorMessage { fieldId = field.fieldId } (always value) ()

        False ->
            []


validateDropDown : Field -> ValidationResult { fieldId : Int }
validateDropDown =
    validateText


validateRadioList : Field -> ValidationResult { fieldId : Int }
validateRadioList =
    validateText


validateQuantity : Field -> ValidationResult { fieldId : Int }
validateQuantity field =
    let
        textValidation =
            validateText field
    in
    if List.isEmpty textValidation then
        validateNumber field

    else
        textValidation


validateNumber : Field -> ValidationResult { fieldId : Int }
validateNumber field =
    let
        maybeIntValue =
            Helpers.getValue field
                |> Maybe.andThen List.head
                |> Maybe.andThen (String.toInt >> Result.toMaybe)

        errorMessage =
            "InvalidNumber.Error"
    in
    Validation.validateMaybeField errorMessage { fieldId = field.fieldId } (always maybeIntValue) ()


validateDate : Field -> ValidationResult { fieldId : Int }
validateDate =
    validateText


validateEmail : Field -> ValidationResult { fieldId : Int }
validateEmail =
    validateText


validatePhone : Field -> ValidationResult { fieldId : Int }
validatePhone =
    validateText


validateZipCode : Field -> ValidationResult { fieldId : Int }
validateZipCode =
    validateText


validateUSState : Field -> ValidationResult { fieldId : Int }
validateUSState =
    validateText
