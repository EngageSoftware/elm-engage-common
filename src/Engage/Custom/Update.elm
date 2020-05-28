module Engage.Custom.Update exposing (updateAnswer, updateFileUploadError, updateFileUploadProgress, updateFileUploadStatus, updateMembershipEventId)

{-| Custom.Update

@docs updateAnswer, updateFileUploadError, updateFileUploadProgress, updateFileUploadStatus, updateMembershipEventId

-}

import Dict
import Engage.Custom.Field as Field
import Engage.Custom.Field.Helpers as Helpers
import Engage.Custom.Form as Form
import Engage.Custom.Section as Section
import Engage.Custom.Types exposing (..)
import Engage.Form.MembershipTypeList as MembershipTypeList
import Engage.UI.Input as Input
import Set exposing (Set)


type alias Query a =
    { a
        | formId : Int
        , sectionId : Int
        , fieldGroupId : Int
        , fieldId : Int
    }


{-| Update an Answer
-}
updateAnswer : Query { a | onlyStateChange : Bool } -> FieldType -> Answer -> Form -> Form
updateAnswer query fieldType answer form =
    let
        updater field =
            if query.fieldId /= field.fieldId then
                field

            else if query.onlyStateChange then
                { field | fieldType = Helpers.updateFieldTypeState { new = fieldType, old = field.fieldType } }

            else
                case Helpers.shouldUpdate field of
                    False ->
                        field

                    True ->
                        case answer of
                            Answer answerData ->
                                { field | fieldType = updateFieldTypeAnswer answerData fieldType }

                            MultipleAnswer multipleAnswer ->
                                { field | fieldType = updateFieldTypeMultipleAnswer multipleAnswer fieldType }

                            BoolAnswer boolAnswer ->
                                { field | fieldType = updateFieldTypeBoolAnswer boolAnswer fieldType }

                            FileAnswer fileInfo ->
                                { field | fieldType = updateFieldTypeFile fileInfo fieldType }

                            MembershipTypeAnswer membershipType ->
                                { field | fieldType = updateFieldTypeMembershipType membershipType fieldType }

        validate =
            if query.onlyStateChange then
                identity

            else
                Form.validate query
    in
    Form.update query updater form
        |> (if Helpers.isCountryFieldType fieldType then
                resetRegions query

            else
                identity
           )
        |> validate


{-| Reset Regions
-}
resetRegions : Query a -> Form -> Form
resetRegions query form =
    let
        regionFieldQueries : List (Query a)
        regionFieldQueries =
            form.sections
                |> Dict.get query.sectionId
                |> Maybe.map
                    (\section ->
                        Section.allFields form section
                            |> List.map (\fieldData -> fieldData.field)
                            |> List.filter Helpers.isRegionField
                            |> List.map (\field -> { query | fieldId = field.fieldId })
                    )
                |> Maybe.withDefault []
    in
    regionFieldQueries
        |> List.foldl (\regionQuery updatedForm -> Form.update regionQuery Field.reset updatedForm) form


{-| Update FileUploadError
-}
updateFileUploadError : FileUploadError -> Form -> Form
updateFileUploadError fileUploadError form =
    let
        updater fileEntryData =
            { fileEntryData | status = Error { message = fileUploadError.errorMessage } }
    in
    Form.updateFileEntryData fileUploadError updater form


{-| Update FileUploadStatus
-}
updateFileUploadStatus : FileUploadStatus -> Form -> Form
updateFileUploadStatus fileUploadStatus form =
    let
        updater fileEntryData =
            { fileEntryData | status = Uploaded }
    in
    Form.updateFileEntryData fileUploadStatus updater form


{-| Update FileUploadProgress
-}
updateFileUploadProgress : FileUploadProgress -> Form -> Form
updateFileUploadProgress fileUploadProgress form =
    let
        updater fileEntryData =
            { fileEntryData | status = Uploading { progressPercentage = fileUploadProgress.progressPercentage } }
    in
    Form.updateFileEntryData fileUploadProgress updater form


updateFieldTypeFile : Input.FileInfo -> FieldType -> FieldType
updateFieldTypeFile fileInfo fieldType =
    case fieldType of
        File data ->
            File { data | entry = updateFilename fileInfo data.entry }

        TextBox data ->
            TextBox data

        LargeTextBox data ->
            LargeTextBox data

        TextArea data ->
            TextArea data

        CheckBox data ->
            CheckBox data

        DropDown data ->
            DropDown data

        RadioList data ->
            RadioList data

        CheckBoxList data ->
            CheckBoxList data

        Quantity data ->
            Quantity data

        Date data ->
            Date data

        Email ->
            Email

        Phone ->
            Phone

        ZipCode ->
            ZipCode

        USState ->
            USState

        Country data ->
            Country data

        Region data ->
            Region data

        Text ->
            Text

        StaticForm _ ->
            fieldType


updateFieldTypeMembershipType : Maybe MembershipTypeList.MembershipType -> FieldType -> FieldType
updateFieldTypeMembershipType membershipType fieldType =
    case fieldType of
        StaticForm (MembershipTypeList data) ->
            StaticForm (MembershipTypeList { data | entry = membershipType })

        _ ->
            fieldType


updateFieldTypeMultipleAnswer : Set String -> FieldType -> FieldType
updateFieldTypeMultipleAnswer answer fieldType =
    case fieldType of
        CheckBoxList data ->
            CheckBoxList { data | entry = updateMultipleEntryData answer data.entry }

        TextBox data ->
            TextBox data

        LargeTextBox data ->
            LargeTextBox data

        TextArea data ->
            TextArea data

        DropDown data ->
            DropDown data

        RadioList data ->
            RadioList data

        CheckBox data ->
            CheckBox data

        Quantity data ->
            Quantity data

        Date data ->
            Date data

        Email ->
            Email

        Phone ->
            Phone

        ZipCode ->
            ZipCode

        USState ->
            USState

        File data ->
            File data

        Region data ->
            Region data

        Country data ->
            Country data

        Text ->
            Text

        StaticForm _ ->
            fieldType


updateFieldTypeBoolAnswer : Bool -> FieldType -> FieldType
updateFieldTypeBoolAnswer answer fieldType =
    case fieldType of
        TextBox data ->
            TextBox data

        LargeTextBox data ->
            LargeTextBox data

        TextArea data ->
            TextArea data

        CheckBox data ->
            CheckBox { data | entry = updateBoolEntryData answer data.entry }

        DropDown data ->
            DropDown data

        RadioList data ->
            RadioList data

        CheckBoxList data ->
            CheckBoxList data

        Quantity data ->
            Quantity data

        Date data ->
            Date data

        Email ->
            Email

        Phone ->
            Phone

        ZipCode ->
            ZipCode

        USState ->
            USState

        File data ->
            File data

        Region data ->
            Region data

        Country data ->
            Country data

        Text ->
            Text

        StaticForm _ ->
            fieldType


updateFieldTypeAnswer : AnswerData -> FieldType -> FieldType
updateFieldTypeAnswer answer fieldType =
    case fieldType of
        TextBox data ->
            TextBox { data | entry = updateEntryData answer data.entry }

        LargeTextBox data ->
            LargeTextBox { data | entry = updateEntryData answer data.entry }

        TextArea data ->
            TextArea { data | entry = updateEntryData answer data.entry }

        CheckBox data ->
            CheckBox data

        DropDown data ->
            DropDown { data | entry = updateEntryData answer data.entry }

        RadioList data ->
            RadioList { data | entry = updateEntryData answer data.entry }

        CheckBoxList data ->
            CheckBoxList data

        Quantity data ->
            Quantity { data | entry = updateEntryData answer data.entry }

        Date data ->
            Date { data | entry = updateEntryData answer data.entry }

        Email ->
            Email

        Phone ->
            Phone

        ZipCode ->
            ZipCode

        USState ->
            USState

        File data ->
            File data

        Region data ->
            Region { data | entry = updateEntryData answer data.entry }

        Country data ->
            Country { data | entry = updateEntryData answer data.entry }

        Text ->
            Text

        StaticForm _ ->
            fieldType


updateFilename : Input.FileInfo -> FileEntryData -> FileEntryData
updateFilename fileInfo fileEntry =
    { fileEntry
        | name = fileInfo.name
        , fileType = fileInfo.fileType
    }


updateMultipleEntryData : Set String -> MultipleEntryData -> MultipleEntryData
updateMultipleEntryData answer entry =
    { entry | values = answer }


updateEntryData : AnswerData -> EntryData -> EntryData
updateEntryData answer entry =
    { entry | value = answer.value }


updateBoolEntryData : Bool -> BoolEntryData -> BoolEntryData
updateBoolEntryData answer entry =
    { entry | value = answer }


{-| Update membership event id
-}
updateMembershipEventId : Int -> Form -> Form
updateMembershipEventId membershipEventid form =
    let
        membershipTypeUpdater data =
            let
                selectedEntry =
                    data.membershipTypeList |> List.filter (\membershipType -> membershipType.value == membershipEventid) |> List.head
            in
            { data | entry = selectedEntry }

        fieldUpdater field =
            case field.fieldType of
                StaticForm (MembershipTypeList data) ->
                    { field | fieldType = StaticForm (MembershipTypeList (membershipTypeUpdater data)) }

                _ ->
                    field
    in
    updateFields fieldUpdater form


updateFields : (Field -> Field) -> Form -> Form
updateFields updater form =
    let
        fieldGroupUpdater fieldGroup =
            { fieldGroup | fields = Dict.map (\_ field -> updater field) fieldGroup.fields }
    in
    updateFieldGroups fieldGroupUpdater form


updateFieldGroups : (FieldGroup -> FieldGroup) -> Form -> Form
updateFieldGroups updater form =
    let
        sectionUpdater : Section -> Section
        sectionUpdater section =
            { section | fieldGroups = Dict.map (\_ fieldGroup -> updater fieldGroup) section.fieldGroups }
    in
    updateSections sectionUpdater form


updateSections : (Section -> Section) -> Form -> Form
updateSections updater form =
    { form | sections = Dict.map (\_ section -> updater section) form.sections }
