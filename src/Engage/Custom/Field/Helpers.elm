module Engage.Custom.Field.Helpers exposing (fieldGroupHasAnswer, fieldHasAnswer, getBoolValue, getFileInfo, getProgressPercentage, getRelativeOrder, getStaticFormTypeText, getText, getValue, getValues, intToDisable, isCountryField, isCountryFieldType, isFileField, isFileFieldType, isRegionField, isRegionFieldType, isSameType, shouldUpdate, toCheckBoxEntryData, toDropdownItem, toDropdownItems, toEntryData, toError, toFileEntryData, toRadioItem, toRadioItems, updateAccordionState, updateDateState, updateDropdownState, updateFieldTypeState, updateInputState, updateOptionsToBool)

import Dict
import Engage.Custom.Types exposing (..)
import Engage.Form.Address as Address
import Engage.UI.Accordion as Accordion
import Engage.UI.Datepicker as Datepicker
import Engage.UI.Dropdown as Dropdown
import Engage.UI.Error as Error exposing (Status)
import Engage.UI.Input as Input
import Engage.Validation as Validation
import Set exposing (Set)
import String



-- HELPERS


toEntryData : Entry -> Maybe EntryData
toEntryData entry =
    case entry of
        Entry entryData ->
            Just entryData

        FileEntry _ ->
            Nothing

        BoolEntry entryData ->
            Nothing


toFileEntryData : Entry -> Maybe FileEntryData
toFileEntryData entry =
    case entry of
        Entry _ ->
            Nothing

        FileEntry entryData ->
            Just entryData

        BoolEntry _ ->
            Nothing


toCheckBoxEntryData : Entry -> Maybe BoolEntryData
toCheckBoxEntryData entry =
    case entry of
        Entry _ ->
            Nothing

        FileEntry _ ->
            Nothing

        BoolEntry entryData ->
            Just entryData


getRelativeOrder : { data | relativeOrder : Int } -> Int
getRelativeOrder { relativeOrder } =
    relativeOrder


intToDisable : Int -> Disable
intToDisable value =
    case value of
        0 ->
            None

        1 ->
            Disabled

        2 ->
            Hidden

        _ ->
            None


updateOptionsToBool : UpdateOptions -> Bool
updateOptionsToBool updateOptions =
    case updateOptions of
        AlwaysUpdate ->
            True

        Update ->
            True

        DontUpdate ->
            False


shouldUpdate : Field -> Bool
shouldUpdate field =
    updateOptionsToBool field.updateOptions


getText : Config msg -> Field -> Maybe (List String)
getText config field =
    case field.fieldType of
        Country _ ->
            getValue field
                |> Maybe.andThen List.head
                |> Maybe.andThen (String.toInt >> Result.toMaybe)
                |> Maybe.andThen (\value -> Dict.get value config.countries)
                |> Maybe.map .countryName
                |> Maybe.map List.singleton

        Region _ ->
            getValue field
                |> Maybe.andThen List.head
                |> Maybe.andThen (String.toInt >> Result.toMaybe)
                |> Maybe.andThen (\value -> Dict.get value (Address.toAllRegions config.regions))
                |> Maybe.map .regionName
                |> Maybe.map List.singleton

        TextBox _ ->
            getValue field

        LargeTextBox _ ->
            getValue field

        TextArea _ ->
            getValue field

        CheckBox _ ->
            getValue field

        DropDown _ ->
            getValue field

        RadioList _ ->
            getValue field

        CheckBoxList _ ->
            getValue field

        Quantity _ ->
            getValue field

        Date _ ->
            getValue field
                |> Maybe.andThen List.head
                |> Maybe.andThen (String.toFloat >> Result.toMaybe)
                -- |> Maybe.map DateHelper.toDateIgnoreTimezone
                |> Maybe.map (Date.Extra.Format.format Date.Extra.Config.Config_en_us.config "%B %e, %Y")
                |> Maybe.map List.singleton

        Email ->
            getValue field

        Phone ->
            getValue field

        ZipCode ->
            getValue field

        USState ->
            getValue field

        File _ ->
            getValue field

        Text ->
            Nothing

        StaticForm staticFormType ->
            getStaticFormTypeText config staticFormType
                |> Maybe.map List.singleton


getStaticFormTypeText : Config msg -> StaticFormType -> Maybe String
getStaticFormTypeText config staticFormType =
    case staticFormType of
        ParticipantForm ->
            Nothing

        MembershipTypeList { entry } ->
            entry |> Maybe.map .name


getBoolValue : Field -> Maybe Bool
getBoolValue field =
    case field.fieldType of
        TextBox fieldData ->
            Nothing

        LargeTextBox fieldData ->
            Nothing

        TextArea fieldData ->
            Nothing

        CheckBox fieldData ->
            Just fieldData.entry.value

        DropDown _ ->
            Nothing

        RadioList _ ->
            Nothing

        CheckBoxList _ ->
            Nothing

        Quantity _ ->
            Nothing

        Date _ ->
            Nothing

        Email ->
            Nothing

        Phone ->
            Nothing

        ZipCode ->
            Nothing

        USState ->
            Nothing

        File _ ->
            Nothing

        Country _ ->
            Nothing

        Region _ ->
            Nothing

        Text ->
            Nothing

        StaticForm _ ->
            Nothing


getValue : Field -> Maybe (List String)
getValue field =
    case field.fieldType of
        TextBox fieldData ->
            Just [ fieldData.entry.value ]

        LargeTextBox fieldData ->
            Just [ fieldData.entry.value ]

        TextArea fieldData ->
            Just [ fieldData.entry.value ]

        CheckBox fieldData ->
            if fieldData.entry.value then
                Just [ field.label ]

            else
                Nothing

        DropDown fieldData ->
            Just [ fieldData.entry.value ]

        RadioList fieldData ->
            Just [ fieldData.entry.value ]

        CheckBoxList fieldData ->
            if Set.isEmpty fieldData.entry.values then
                Nothing

            else
                Just (fieldData.entry.values |> Set.toList)

        Quantity fieldData ->
            Just [ fieldData.entry.value ]

        Date fieldData ->
            Just [ fieldData.entry.value ]

        Email ->
            Nothing

        Phone ->
            Nothing

        ZipCode ->
            Nothing

        USState ->
            Nothing

        File fieldData ->
            Just [ fieldData.entry.name ]

        Country fieldData ->
            Just [ fieldData.entry.value ]

        Region fieldData ->
            Just [ fieldData.entry.value ]

        Text ->
            Nothing

        StaticForm (MembershipTypeList data) ->
            data.entry |> Maybe.map .name |> Maybe.map List.singleton

        StaticForm ParticipantForm ->
            Nothing


getProgressPercentage : FileStatus -> Maybe Float
getProgressPercentage fileStatus =
    case fileStatus of
        NoFile ->
            Nothing

        Uploading { progressPercentage } ->
            Just progressPercentage

        Uploaded ->
            Nothing

        Error _ ->
            Nothing


isCountryField : Field -> Bool
isCountryField field =
    isCountryFieldType field.fieldType


isRegionFieldType : FieldType -> Bool
isRegionFieldType fieldType =
    case fieldType of
        Region _ ->
            True

        _ ->
            False


isRegionField : Field -> Bool
isRegionField field =
    isRegionFieldType field.fieldType


isCountryFieldType : FieldType -> Bool
isCountryFieldType fieldType =
    case fieldType of
        Country _ ->
            True

        _ ->
            False


isFileField : Field -> Bool
isFileField field =
    isFileFieldType field.fieldType


isFileFieldType : FieldType -> Bool
isFileFieldType fieldType =
    case fieldType of
        File _ ->
            True

        _ ->
            False


getValues : Field -> Maybe (Set String)
getValues field =
    case field.fieldType of
        CheckBoxList fieldData ->
            Just fieldData.entry.values

        File fieldData ->
            Nothing

        TextBox fieldData ->
            Nothing

        LargeTextBox fieldData ->
            Nothing

        TextArea fieldData ->
            Nothing

        CheckBox _ ->
            Nothing

        DropDown fieldData ->
            Nothing

        RadioList _ ->
            Nothing

        Quantity _ ->
            Nothing

        Date fieldData ->
            Nothing

        Email ->
            Nothing

        Phone ->
            Nothing

        ZipCode ->
            Nothing

        USState ->
            Nothing

        Region fieldData ->
            Nothing

        Country fieldData ->
            Nothing

        Text ->
            Nothing

        StaticForm _ ->
            Nothing


getFileInfo : Field -> Maybe Input.FileInfo
getFileInfo field =
    case field.fieldType of
        File fieldData ->
            Just
                { name = fieldData.entry.name
                , fileType = fieldData.entry.fileType
                , progressPercentage = getProgressPercentage fieldData.entry.status
                }

        TextBox fieldData ->
            Nothing

        LargeTextBox fieldData ->
            Nothing

        TextArea fieldData ->
            Nothing

        CheckBox _ ->
            Nothing

        DropDown fieldData ->
            Nothing

        RadioList _ ->
            Nothing

        CheckBoxList _ ->
            Nothing

        Quantity _ ->
            Nothing

        Date fieldData ->
            Nothing

        Email ->
            Nothing

        Phone ->
            Nothing

        ZipCode ->
            Nothing

        USState ->
            Nothing

        Region fieldData ->
            Nothing

        Country fieldData ->
            Nothing

        Text ->
            Nothing

        StaticForm _ ->
            Nothing


toError : ValidationResult { fieldId : Int } -> Field -> Status
toError validationErrors field =
    case Validation.findErrorMessage { fieldId = field.fieldId } validationErrors of
        Nothing ->
            Error.None { infos = [] }

        Just reason ->
            Error.Error { reasons = [ reason ] }


toRadioItems : List FieldChoice -> List { id : String, text : String }
toRadioItems fieldChoices =
    fieldChoices
        |> List.sortBy .relativeOrder
        |> List.map toRadioItem


toRadioItem : FieldChoice -> { id : String, text : String }
toRadioItem fieldChoice =
    { id = fieldChoice.value, text = fieldChoice.value }


toDropdownItems : List FieldChoice -> List Dropdown.Item
toDropdownItems fieldChoices =
    fieldChoices
        |> List.sortBy .relativeOrder
        |> List.filterMap toDropdownItem


toDropdownItem : FieldChoice -> Maybe Dropdown.Item
toDropdownItem fieldChoice =
    fieldChoice.fieldChoiceId
        |> Maybe.map (always { value = fieldChoice.value, text = fieldChoice.name, enabled = True })


fieldHasAnswer : Field -> Bool
fieldHasAnswer field =
    field
        |> getValue
        |> Maybe.map (always True)
        |> Maybe.withDefault False


fieldGroupHasAnswer : FieldGroup -> Bool
fieldGroupHasAnswer { fields } =
    fields
        |> Dict.values
        |> List.any fieldHasAnswer


isSameType : FieldType -> FieldType -> Bool
isSameType a b =
    case a of
        TextBox _ ->
            case b of
                TextBox _ ->
                    True

                _ ->
                    False

        LargeTextBox _ ->
            case b of
                LargeTextBox _ ->
                    True

                _ ->
                    False

        TextArea _ ->
            case b of
                TextArea _ ->
                    True

                _ ->
                    False

        CheckBox _ ->
            case b of
                CheckBox _ ->
                    True

                _ ->
                    False

        DropDown _ ->
            case b of
                DropDown _ ->
                    True

                _ ->
                    False

        RadioList _ ->
            case b of
                RadioList _ ->
                    True

                _ ->
                    False

        CheckBoxList _ ->
            case b of
                CheckBoxList _ ->
                    True

                _ ->
                    False

        Quantity _ ->
            case b of
                Quantity _ ->
                    True

                _ ->
                    False

        Date _ ->
            case b of
                Date _ ->
                    True

                _ ->
                    False

        Email ->
            case b of
                Email ->
                    True

                _ ->
                    False

        Phone ->
            case b of
                Phone ->
                    True

                _ ->
                    False

        ZipCode ->
            case b of
                ZipCode ->
                    True

                _ ->
                    False

        USState ->
            case b of
                USState ->
                    True

                _ ->
                    False

        Region _ ->
            case b of
                Region _ ->
                    True

                _ ->
                    False

        Country _ ->
            case b of
                Country _ ->
                    True

                _ ->
                    False

        Text ->
            case b of
                Text ->
                    True

                _ ->
                    False

        StaticForm a ->
            case b of
                StaticForm _ ->
                    True

                _ ->
                    False

        File _ ->
            case b of
                File _ ->
                    True

                _ ->
                    False


updateInputState : Input.State -> FieldType -> FieldType
updateInputState inputState fieldType =
    case fieldType of
        TextBox data ->
            TextBox { data | state = inputState }

        LargeTextBox data ->
            LargeTextBox { data | state = inputState }

        TextArea data ->
            TextArea { data | state = inputState }

        DropDown data ->
            DropDown data

        Date data ->
            Date data

        CheckBox data ->
            CheckBox { data | state = inputState }

        RadioList data ->
            RadioList { data | state = inputState }

        CheckBoxList data ->
            CheckBoxList { data | state = inputState }

        Quantity data ->
            Quantity { data | state = inputState }

        Email ->
            Email

        Phone ->
            Phone

        ZipCode ->
            ZipCode

        USState ->
            USState

        File data ->
            File { data | state = inputState }

        Country data ->
            Country data

        Region data ->
            Region data

        Text ->
            Text

        StaticForm _ ->
            fieldType


updateAccordionState : Accordion.State -> FieldType -> FieldType
updateAccordionState accordionState fieldType =
    case fieldType of
        StaticForm (MembershipTypeList data) ->
            StaticForm (MembershipTypeList { data | state = accordionState })

        _ ->
            fieldType


updateFieldTypeState : { old : FieldType, new : FieldType } -> FieldType
updateFieldTypeState { old, new } =
    case old of
        TextBox oldData ->
            case new of
                TextBox newData ->
                    TextBox { oldData | state = newData.state }

                _ ->
                    old

        LargeTextBox oldData ->
            case new of
                LargeTextBox newData ->
                    LargeTextBox { oldData | state = newData.state }

                _ ->
                    old

        TextArea oldData ->
            case new of
                TextArea newData ->
                    TextArea { oldData | state = newData.state }

                _ ->
                    old

        DropDown oldData ->
            case new of
                DropDown newData ->
                    DropDown { oldData | state = newData.state }

                _ ->
                    old

        Date oldData ->
            case new of
                Date newData ->
                    Date { oldData | state = newData.state }

                _ ->
                    old

        CheckBox oldData ->
            case new of
                CheckBox newData ->
                    CheckBox { oldData | state = newData.state }

                _ ->
                    old

        RadioList oldData ->
            case new of
                RadioList newData ->
                    RadioList { oldData | state = newData.state }

                _ ->
                    old

        CheckBoxList oldData ->
            case new of
                CheckBoxList newData ->
                    CheckBoxList { oldData | state = newData.state }

                _ ->
                    old

        Quantity oldData ->
            case new of
                Quantity newData ->
                    Quantity { oldData | state = newData.state }

                _ ->
                    old

        Email ->
            case new of
                Email ->
                    Email

                _ ->
                    old

        Phone ->
            case new of
                Phone ->
                    Phone

                _ ->
                    old

        ZipCode ->
            case new of
                ZipCode ->
                    ZipCode

                _ ->
                    old

        USState ->
            case new of
                USState ->
                    USState

                _ ->
                    old

        File oldData ->
            case new of
                File newData ->
                    File { oldData | state = newData.state }

                _ ->
                    old

        Region oldData ->
            case new of
                Region newData ->
                    Region { oldData | state = newData.state }

                _ ->
                    old

        Country oldData ->
            case new of
                Country newData ->
                    Country { oldData | state = newData.state }

                _ ->
                    old

        Text ->
            old

        StaticForm (MembershipTypeList oldData) ->
            case new of
                StaticForm (MembershipTypeList newData) ->
                    StaticForm (MembershipTypeList { oldData | state = newData.state })

                _ ->
                    old

        StaticForm ParticipantForm ->
            Debug.crash "Not Implemented"


updateDropdownState : Dropdown.State -> FieldType -> FieldType
updateDropdownState dropdownState fieldType =
    case fieldType of
        TextBox data ->
            fieldType

        LargeTextBox data ->
            fieldType

        TextArea data ->
            fieldType

        DropDown data ->
            DropDown { data | state = dropdownState }

        Date data ->
            fieldType

        CheckBox data ->
            fieldType

        RadioList data ->
            fieldType

        CheckBoxList data ->
            fieldType

        Quantity data ->
            fieldType

        Email ->
            fieldType

        Phone ->
            fieldType

        ZipCode ->
            fieldType

        USState ->
            fieldType

        File data ->
            fieldType

        Country data ->
            Country { data | state = dropdownState }

        Region data ->
            Region { data | state = dropdownState }

        Text ->
            fieldType

        StaticForm _ ->
            fieldType


updateDateState : Datepicker.State -> FieldType -> FieldType
updateDateState datepickerState fieldType =
    case fieldType of
        TextBox data ->
            fieldType

        LargeTextBox data ->
            fieldType

        TextArea data ->
            fieldType

        DropDown data ->
            fieldType

        Date data ->
            Date { data | state = datepickerState }

        CheckBox data ->
            fieldType

        RadioList data ->
            fieldType

        CheckBoxList data ->
            fieldType

        Quantity data ->
            fieldType

        Email ->
            fieldType

        Phone ->
            fieldType

        ZipCode ->
            fieldType

        USState ->
            fieldType

        File data ->
            fieldType

        Country data ->
            fieldType

        Region data ->
            fieldType

        Text ->
            fieldType

        StaticForm _ ->
            fieldType
