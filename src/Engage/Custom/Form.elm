module Engage.Custom.Form exposing
    ( allFields
    , completedView
    , findField
    , form
    , isValid
    , update
    , updateFileEntryData
    , validate
    , validateAll
    , view
    )

import Dict exposing (Dict)
import Engage.Custom.Form.Css as Css
import Engage.Custom.Section as Section exposing (sectionTupleDecoder)
import Engage.Custom.Types exposing (..)
import Engage.Html.Extra as HtmlExtra
import Engage.Namespace as Namespace
import Engage.Validation as Validation exposing (ValidationErrors)
import Html exposing (Html)
import Html.CssHelpers


{ class } =
    Namespace.engagecore
        |> Namespace.toString
        |> Html.CssHelpers.withNamespace


view : Config msg -> Form -> Html msg
view config form =
    let
        showSectionName =
            Dict.size form.sections > 1
    in
    form.sections
        |> Dict.values
        |> List.sortBy .relativeOrder
        |> List.map (Section.view config)
        |> Html.div [ class [ Css.Sections ] ]


form : Config msg -> Form -> Html msg
form config form =
    let
        showSectionName =
            Dict.size form.sections > 1

        sections =
            form.sections
                |> Dict.values
                |> List.sortBy .relativeOrder
                |> List.map (Section.form { config = config, validations = form.validations, showSectionName = showSectionName } form)

        title =
            if Dict.isEmpty form.sections then
                HtmlExtra.none

            else
                Html.h3 [] [ Html.text form.name ]
    in
    Html.div [ class [ Css.Form ] ] (title :: sections)


completedView : Config msg -> Form -> Html msg
completedView config form =
    let
        showSectionName =
            Dict.size form.sections > 1
    in
    form.sections
        |> Dict.values
        |> List.map
            (Section.completedView
                { config = config
                , validations = form.validations
                , showName = showSectionName
                }
            )
        |> Html.div [ class [ Css.Sections ] ]


type alias Query a =
    { a | formId : Int, sectionId : Int, fieldGroupId : Int, fieldId : Int }


update : Query a -> (Field -> Field) -> Form -> Form
update query updater form =
    { form | sections = Dict.update query.sectionId (Maybe.map <| Section.update query updater form) form.sections }


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


validate : { a | fieldId : Int } -> Form -> Form
validate fieldId form =
    { form
        | validations =
            form.sections
                |> Dict.values
                |> List.concatMap (Section.validate fieldId)
                |> Validation.merge (\( { fieldId }, status ) -> fieldId) (cleanValidations fieldId form.validations)
    }


cleanValidations : { a | fieldId : Int } -> ValidationErrors { fieldId : Int } -> ValidationErrors { fieldId : Int }
cleanValidations fieldId validations =
    validations |> List.filter (\( currentFieldId, _ ) -> currentFieldId.fieldId /= fieldId.fieldId)


validateAll : Form -> Form
validateAll form =
    { form
        | validations =
            form.sections
                |> Dict.values
                |> List.concatMap Section.validateAll
    }


allFields : Form -> List ( Form, Section, FieldGroup, Field )
allFields form =
    form.sections
        |> Dict.values
        |> List.concatMap (Section.allFields form)


findField : { a | formId : Int, sectionId : Int, fieldGroupId : Int, fieldId : Int } -> Form -> Maybe ( Form, Section, FieldGroup, Field )
findField query form =
    form.sections
        |> Dict.get query.sectionId
        |> Maybe.andThen (Section.findField query form)


isValid : Form -> Bool
isValid form =
    Validation.isValid form.validations
