module Engage.Custom.Field exposing
    ( FieldData
    , allFields
    , findField
    , getFileEntryData
    , namespacedId
    , reset
    , update
    )

import Dict exposing (Dict)
import Engage.Custom.Types exposing (..)
import Engage.Namespace as Namespace
import Set exposing (Set)


engagecoreNamespace : String
engagecoreNamespace =
    Namespace.toString Namespace.engagecore


type alias Query a =
    { a | fieldId : Int }


type alias FieldData =
    { form : Form
    , section : Section
    , fieldGroup : FieldGroup
    , field : Field
    }


update : Query a -> (Field -> Field) -> Form -> Section -> FieldGroup -> FieldGroup
update query updater form section fieldGroup =
    { fieldGroup | fields = Dict.update query.fieldId (Maybe.map updater) fieldGroup.fields }


namespacedId : Field -> String
namespacedId field =
    engagecoreNamespace ++ "Field" ++ String.fromInt field.fieldId


allFields : Form -> Section -> FieldGroup -> List FieldData
allFields form section fieldGroup =
    fieldGroup.fields
        |> Dict.values
        |> List.map (\field -> { form = form, section = section, fieldGroup = fieldGroup, field = field })


getFileEntryData : Field -> Maybe FileEntryData
getFileEntryData field =
    case field.fieldType of
        File { entry } ->
            Just entry

        _ ->
            Nothing


findField : { a | formId : Int, sectionId : Int, fieldGroupId : Int, fieldId : Int } -> Form -> Section -> FieldGroup -> Maybe FieldData
findField query form section fieldGroup =
    fieldGroup.fields
        |> Dict.get query.fieldId
        |> Maybe.map (\field -> { form = form, section = section, fieldGroup = fieldGroup, field = field })


reset : Field -> Field
reset field =
    let
        emptyMultipleEntryData =
            { values = Set.empty }

        emptyEntryData =
            { value = "" }

        emptyFileEntryData =
            { name = ""
            , fileType = ""
            , status = NoFile
            }

        emptyCheckBoxEntryData =
            { value = False }

        updatedFieldType =
            case field.fieldType of
                TextBox data ->
                    TextBox { data | entry = emptyEntryData }

                LargeTextBox data ->
                    LargeTextBox { data | entry = emptyEntryData }

                TextArea data ->
                    TextArea { data | entry = emptyEntryData }

                CheckBox data ->
                    CheckBox { data | entry = emptyCheckBoxEntryData }

                DropDown data ->
                    DropDown { data | entry = emptyEntryData }

                RadioList data ->
                    RadioList { data | entry = emptyEntryData }

                CheckBoxList data ->
                    CheckBoxList { data | entry = emptyMultipleEntryData }

                Quantity data ->
                    Quantity { data | entry = emptyEntryData }

                Date data ->
                    Date { data | entry = emptyEntryData }

                Email ->
                    Email

                Phone ->
                    Phone

                ZipCode ->
                    ZipCode

                USState ->
                    USState

                File data ->
                    File { data | entry = emptyFileEntryData }

                Region data ->
                    Region { data | entry = emptyEntryData }

                Country data ->
                    Country { data | entry = emptyEntryData }

                Text ->
                    Text

                StaticForm _ ->
                    field.fieldType
    in
    { field | fieldType = updatedFieldType }
