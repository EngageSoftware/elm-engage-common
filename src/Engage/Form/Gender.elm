module Engage.Form.Gender exposing
    ( Attribute
    , field
    , form
    , localization
    , onChange
    )

import Dict
import Engage.Entity.Gender as Gender exposing (Gender(..))
import Engage.Form.Field as Field
import Engage.Localization as Localization exposing (Localization)
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.String exposing (space)
import Engage.UI.Attribute as Attribute
import Engage.UI.Input as Input
import Engage.UI.Message as Message
import Engage.UI.MessageType as MessageType
import Engage.Validation as Validation exposing (ValidationErrors)
import Html exposing (Html)


type InternalAttribute field msg
    = InternalAttribute
        { namespace : Namespace
        , onChange : Maybe (ValidationErrors field -> { onlyStateChange : Bool } -> Input.State -> Gender -> msg)
        , localization : Localization
        , required : Bool
        , field : Maybe field
        }


emptyAttribute : InternalAttribute field msg
emptyAttribute =
    InternalAttribute
        { namespace = Namespace.engagecore
        , onChange = Nothing
        , localization = Dict.empty
        , required = False
        , field = Nothing
        }


type alias Attribute field msg =
    InternalAttribute field msg -> InternalAttribute field msg


field : field -> Attribute field msg
field value =
    \(InternalAttribute attribute) -> InternalAttribute { attribute | field = Just value }


onChange : (ValidationErrors field -> { onlyStateChange : Bool } -> Input.State -> Gender -> msg) -> Attribute field msg
onChange value =
    \(InternalAttribute attribute) -> InternalAttribute { attribute | onChange = Just value }


localization : Localization -> Attribute field msg
localization value =
    \(InternalAttribute attribute) -> InternalAttribute { attribute | localization = value }


missingText : String
missingText =
    "Missing required attribute for Gender.form:"


form : List (Attribute field msg) -> ValidationErrors field -> Input.State -> Gender -> Html msg
form attributes validations state gender =
    let
        (InternalAttribute attribute) =
            Attribute.process emptyAttribute attributes
    in
    case attribute.onChange of
        Nothing ->
            error attribute.namespace "onChange"

        Just onChange ->
            case attribute.field of
                Nothing ->
                    error attribute.namespace "field"

                Just field ->
                    internalForm (InternalAttribute attribute) onChange field validations state gender


error : Namespace -> String -> Html msg
error namespace value =
    Message.message
        { namespace = namespace
        , messageType = MessageType.Error
        }
        [ Html.text (missingText |> space value)
        ]


internalForm :
    InternalAttribute field msg
    -> (ValidationErrors field -> { onlyStateChange : Bool } -> Input.State -> Gender -> msg)
    -> field
    -> ValidationErrors field
    -> Input.State
    -> Gender
    -> Html msg
internalForm (InternalAttribute attribute) onChange field validations state gender =
    let
        radioListOnChange validations onlyStateChange state genderString =
            onChange validations onlyStateChange state (Gender.fromString genderString)
    in
    Field.radioListField
        { namespace = attribute.namespace
        , onChange = radioListOnChange
        , localization = attribute.localization
        , field = field
        , required = attribute.required
        , items =
            [ Male, Female, Other, Unspecified ]
                |> List.map (Gender.toString >> toItem attribute)
                |> List.filterMap identity
        }
        validations
        state
        (gender |> Gender.toString)


toItem : { a | localization : Localization } -> String -> Maybe { id : String, text : String }
toItem args gender =
    let
        localizedGender =
            if String.isEmpty gender then
                Localization.localizeStringWithDefault "" "Unspecified.Gender" args

            else
                Localization.localizeString (gender ++ ".Gender") args
    in
    case localizedGender of
        "" ->
            Nothing

        _ ->
            Just
                { id = gender
                , text = localizedGender
                }
