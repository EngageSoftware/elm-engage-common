module Engage.Custom.Section exposing
    ( allFields
    , completedView
    , findField
    , form
    , sectionDecoder
    , sectionTupleDecoder
    , update
    , validate
    , validateAll
    , view
    )

import Dict
import Engage.CssHelpers
import Engage.Custom.Field as Field
import Engage.Custom.Field.Helpers as Field
import Engage.Custom.Field.Json exposing (fieldGroupDecoder)
import Engage.Custom.Field.Validation as Field
import Engage.Custom.Field.View as Field
import Engage.Custom.Types exposing (..)
import Engage.Html.Extra as HtmlExtra
import Engage.Namespace as Namespace
import Engage.Validation exposing (ValidationErrors)
import Html exposing (Html)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


class =
    Namespace.engagecore
        |> Namespace.toString
        |> Engage.CssHelpers.withNamespace


type alias Query a =
    { a | sectionId : Int, fieldId : Int, fieldGroupId : Int }


view : Config msg -> Section -> Html msg
view config section =
    Html.div []
        (section.fieldGroups
            |> Dict.values
            |> List.sortBy Field.getRelativeOrder
            |> List.filter Field.fieldGroupHasAnswer
            |> List.concatMap (\fieldGroup -> Field.view config fieldGroup)
        )


form : { a | config : Config msg, validations : ValidationErrors { fieldId : Int }, showSectionName : Bool } -> Form -> Section -> Html msg
form args form section =
    let
        legend =
            if args.showSectionName then
                Html.legend [] [ Html.text section.name ]

            else
                HtmlExtra.none
    in
    Html.fieldset [ class [ "FormSection" ] ]
        (legend
            :: (section.fieldGroups
                    |> Dict.values
                    |> List.sortBy Field.getRelativeOrder
                    |> List.map (Field.fieldGroupForm args ( form, section ))
               )
        )


completedView : { config : Config msg, validations : ValidationErrors { fieldId : Int }, showName : Bool } -> Section -> Html msg
completedView args section =
    Html.ul []
        (nameView args section
            :: (section.fieldGroups
                    |> Dict.values
                    |> List.sortBy Field.getRelativeOrder
                    |> List.filter Field.fieldGroupHasAnswer
                    |> List.concatMap (\fieldGroup -> Field.viewCompletedEntries args fieldGroup)
               )
        )


nameView : { a | showName : Bool } -> Section -> Html msg
nameView args section =
    if args.showName then
        Html.li [] [ Html.strong [] [ Html.text section.name ] ]

    else
        HtmlExtra.none


update : Query a -> (Field -> Field) -> Form -> Section -> Section
update query updater form section =
    { section | fieldGroups = Dict.update query.fieldGroupId (Maybe.map <| Field.update query updater form section) section.fieldGroups }


validate : { a | fieldId : Int } -> Section -> ValidationErrors { fieldId : Int }
validate fieldId section =
    section.fieldGroups
        |> Dict.values
        |> List.concatMap (Field.validateFieldGroup fieldId)


validateAll : Section -> ValidationErrors { fieldId : Int }
validateAll section =
    section.fieldGroups
        |> Dict.values
        |> List.concatMap Field.validateAllFieldGroup



-- Decoders


sectionDecoder : Date -> Decode.Decoder Section
sectionDecoder now =
    decode Section
        |> required "formSectionId" int
        |> required "name" string
        |> required "relativeOrder" int
        |> required "optional" bool
        |> required "optionalLabel" string
        |> required "adminOnly" bool
        |> required "fieldGroups" (list (fieldGroupDecoder now) |> map Dict.fromList)


sectionTupleDecoder : Date -> Decode.Decoder ( Int, Section )
sectionTupleDecoder now =
    decode (\a b -> ( a, b ))
        |> required "formSectionId" int
        |> custom (sectionDecoder now)


allFields : Form -> Section -> List ( Form, Section, FieldGroup, Field )
allFields form section =
    section.fieldGroups
        |> Dict.values
        |> List.concatMap (Field.allFields form section)


findField : { a | formId : Int, sectionId : Int, fieldGroupId : Int, fieldId : Int } -> Form -> Section -> Maybe ( Form, Section, FieldGroup, Field )
findField query form section =
    section.fieldGroups
        |> Dict.get query.fieldGroupId
        |> Maybe.andThen (Field.findField query form section)
