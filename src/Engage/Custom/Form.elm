module Engage.Custom.Form exposing (allFields, completedView, findField, formView, isValid, update, updateFileEntryData, validate, validateAll, view)

{-| Custom.Form

@docs allFields, completedView, findField, formView, isValid, update, updateFileEntryData, validate, validateAll, view

-}

import Dict exposing (Dict)
import Engage.CssHelpers
import Engage.Custom.Field exposing (FieldData)
import Engage.Custom.Section as Section exposing (sectionTupleDecoder)
import Engage.Custom.Types exposing (..)
import Engage.Html.Extra as HtmlExtra
import Engage.Namespace as Namespace
import Engage.Validation as Validation exposing (ValidationErrors)
import Html exposing (Html)


class =
    Namespace.engagecore
        |> Namespace.toString
        |> Engage.CssHelpers.withNamespace


{-| Get the view
-}
view : Config msg -> Form -> Html msg
view config formValue =
    let
        showSectionName =
            Dict.size formValue.sections > 1
    in
    formValue.sections
        |> Dict.values
        |> List.sortBy .relativeOrder
        |> List.map (Section.view config)
        |> Html.div [ class [ "Sections" ] ]


{-| Get the form view
-}
formView : Config msg -> Form -> Html msg
formView config formValue =
    let
        showSectionName =
            Dict.size formValue.sections > 1

        sections =
            formValue.sections
                |> Dict.values
                |> List.sortBy .relativeOrder
                |> List.map (Section.form { config = config, validations = formValue.validations, showSectionName = showSectionName } formValue)

        title =
            if Dict.isEmpty formValue.sections then
                HtmlExtra.none

            else
                Html.h3 [] [ Html.text formValue.name ]
    in
    Html.div [ class [ "Form" ] ] (title :: sections)


{-| Get the completed view
-}
completedView : Config msg -> Form -> Html msg
completedView config formValue =
    let
        showSectionName =
            Dict.size formValue.sections > 1
    in
    formValue.sections
        |> Dict.values
        |> List.map
            (Section.completedView
                { config = config
                , validations = formValue.validations
                , showName = showSectionName
                }
            )
        |> Html.div [ class [ "Sections" ] ]


type alias Query a =
    { a | formId : Int, sectionId : Int, fieldGroupId : Int, fieldId : Int }


{-| Update a Form field
-}
update : Query a -> (Field -> Field) -> Form -> Form
update query updater form =
    { form | sections = Dict.update query.sectionId (Maybe.map <| Section.update query updater form) form.sections }


{-| Update a Form FileEntryData
-}
updateFileEntryData : Query a -> (FileEntryData -> FileEntryData) -> Form -> Form
updateFileEntryData query updater form =
    let
        fieldUpdater field =
            case field.fieldType of
                File data ->
                    { field | fieldType = File { data | entry = updater data.entry } }

                _ ->
                    field
    in
    update query fieldUpdater form


{-| Validate a Form
-}
validate : { a | fieldId : Int } -> Form -> Form
validate fieldId form =
    { form
        | validations =
            form.sections
                |> Dict.values
                |> List.concatMap (Section.validate fieldId)
                |> Validation.merge (\( fields, status ) -> fields.fieldId) (cleanValidations fieldId form.validations)
    }


{-| Clear validations
-}
cleanValidations : { a | fieldId : Int } -> ValidationErrors { fieldId : Int } -> ValidationErrors { fieldId : Int }
cleanValidations fieldId validations =
    validations |> List.filter (\( currentFieldId, _ ) -> currentFieldId.fieldId /= fieldId.fieldId)


{-| Validate all fields
-}
validateAll : Form -> Form
validateAll form =
    { form
        | validations =
            form.sections
                |> Dict.values
                |> List.concatMap Section.validateAll
    }


{-| Get all fields of the Form
-}
allFields : Form -> List FieldData
allFields form =
    form.sections
        |> Dict.values
        |> List.concatMap (Section.allFields form)


{-| Field a Form field
-}
findField : { a | formId : Int, sectionId : Int, fieldGroupId : Int, fieldId : Int } -> Form -> Maybe FieldData
findField query form =
    form.sections
        |> Dict.get query.sectionId
        |> Maybe.andThen (Section.findField query form)


{-| Check if the Form is valid
-}
isValid : Form -> Bool
isValid form =
    Validation.isValid form.validations
