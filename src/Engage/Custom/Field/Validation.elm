module Engage.Custom.Field.Validation exposing (errorMessage, validateAllFieldGroup, validateCheckBox, validateCheckBoxList, validateDate, validateDropDown, validateEmail, validateField, validateFieldGroup, validateFile, validateNumber, validatePhone, validateQuantity, validateRadioList, validateText, validateUSState, validateZipCode)

import Dict
import Engage.Custom.Field.Helpers as Helpers
import Engage.Custom.Types exposing (..)
import Engage.Validation as Validation exposing (ValidationErrors)
import Set
import String


validateFieldGroup : { a | fieldId : Int } -> FieldGroup -> ValidationErrors { fieldId : Int }
validateFieldGroup fieldId fieldSet =
    fieldSet.fields
        |> Dict.values
        |> List.concatMap (validateField fieldId)


validateAllFieldGroup : FieldGroup -> ValidationErrors { fieldId : Int }
validateAllFieldGroup fieldSet =
    fieldSet.fields
        |> Dict.values
        |> List.concatMap (\fieldData -> validateField { fieldId = fieldData.fieldId } fieldData)


validateField : { a | fieldId : Int } -> Field -> ValidationErrors { fieldId : Int }
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

            StaticForm _ ->
                []

    else
        []


validateCheckBoxList : Field -> ValidationErrors { fieldId : Int }
validateCheckBoxList field =
    case field.required of
        True ->
            let
                values =
                    Helpers.getValues field
                        |> Maybe.map Set.toList
                        |> Maybe.withDefault []
            in
            Validation.validateField [ Validation.validateListNotEmptyField (errorMessage field) { fieldId = field.fieldId } (always values) ] ()

        False ->
            []


validateText : Field -> ValidationErrors { fieldId : Int }
validateText field =
    case field.required of
        True ->
            let
                value =
                    Helpers.getValue field
                        |> Maybe.andThen List.head
            in
            Validation.validateField [ Validation.validateMaybeStringField (errorMessage field) { fieldId = field.fieldId } (always value) ] ()

        False ->
            []


errorMessage : Field -> String
errorMessage field =
    if String.isEmpty field.errorMessage then
        field.label ++ ".Required"

    else
        field.errorMessage


validateFile : Field -> ValidationErrors { fieldId : Int }
validateFile field =
    case field.required of
        True ->
            let
                fileInfo =
                    Helpers.getFileInfo field
                        |> Maybe.map .name
            in
            Validation.validateField
                [ Validation.validateMaybeStringField field.errorMessage
                    { fieldId = field.fieldId }
                    (always fileInfo)
                ]
                ()

        False ->
            []


validateCheckBox : Field -> ValidationErrors { fieldId : Int }
validateCheckBox field =
    case field.required of
        True ->
            let
                value =
                    Helpers.getBoolValue field |> Maybe.withDefault False
            in
            Validation.validateField [ Validation.validateBoolField field.errorMessage { fieldId = field.fieldId } (always value) ] ()

        False ->
            []


validateDropDown : Field -> ValidationErrors { fieldId : Int }
validateDropDown =
    validateText


validateRadioList : Field -> ValidationErrors { fieldId : Int }
validateRadioList =
    validateText


validateQuantity : Field -> ValidationErrors { fieldId : Int }
validateQuantity field =
    let
        textValidation =
            validateText field
    in
    if List.isEmpty textValidation then
        validateNumber field

    else
        textValidation


validateNumber : Field -> ValidationErrors { fieldId : Int }
validateNumber field =
    let
        maybeIntValue =
            Helpers.getValue field
                |> Maybe.andThen List.head
                |> Maybe.andThen String.toInt

        errorMessageValue =
            "InvalidNumber.Error"
    in
    Validation.validateField [ Validation.validateMaybeField errorMessageValue { fieldId = field.fieldId } (always maybeIntValue) ] ()


validateDate : Field -> ValidationErrors { fieldId : Int }
validateDate =
    validateText


validateEmail : Field -> ValidationErrors { fieldId : Int }
validateEmail =
    validateText


validatePhone : Field -> ValidationErrors { fieldId : Int }
validatePhone =
    validateText


validateZipCode : Field -> ValidationErrors { fieldId : Int }
validateZipCode =
    validateText


validateUSState : Field -> ValidationErrors { fieldId : Int }
validateUSState =
    validateText
