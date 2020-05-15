module Engage.Validation exposing
    ( RemoteValidationErrors
    , ValidationErrors
    , ValidationStatus(..)
    , errors
    , fieldError
    , fields
    , filter
    , findErrorMessage
    , isInvalid
    , isInvalidField
    , isValid
    , isValidField
    , localize
    , merge
    , toError
    , validateBoolField
    , validateDependentMaybeField
    , validateDependentStringField
    , validateField
    , validateListNotEmptyField
    , validateMaybeField
    , validateMaybeStringField
    , validateStringField
    )

import Dict
import Dict.Extra
import Engage.Localization as Localization exposing (Localization)
import Engage.UI.Error as Error exposing (Status)
import RemoteData exposing (WebData)
import Validate exposing (ifBlank, ifInvalid, ifNothing)


type ValidationStatus
    = Valid
    | Invalid String
    | Ignored


type alias ValidationErrors field =
    List ( field, ValidationStatus )


type alias RemoteValidationErrors =
    WebData (List String)


toError : ValidationErrors a -> Status
toError validations =
    case errors validations of
        [] ->
            Error.None { infos = [] }

        reasons ->
            Error.Error { reasons = reasons }


fieldError : Localization -> field -> ValidationErrors field -> Status
fieldError localization field validations =
    case findErrorMessage field validations of
        Nothing ->
            Error.None { infos = [] }

        Just error ->
            Error.Error { reasons = [ Localization.localizeStringWithDefault error error { localization = localization } ] }


errors : ValidationErrors field -> List String
errors validations =
    validations
        |> List.map Tuple.second
        |> List.filterMap getErrorMessage


isValidStatus : ValidationStatus -> Bool
isValidStatus validationStatus =
    case validationStatus of
        Valid ->
            True

        _ ->
            False


isInvalidStatus : ValidationStatus -> Bool
isInvalidStatus validationStatus =
    case validationStatus of
        Valid ->
            False

        Invalid _ ->
            True

        Ignored ->
            False


getErrorMessage : ValidationStatus -> Maybe String
getErrorMessage validationStatus =
    case validationStatus of
        Invalid msg ->
            Just msg

        Valid ->
            Nothing

        Ignored ->
            Nothing


fields : ValidationErrors field -> List field
fields =
    List.map Tuple.first


isValid : ValidationErrors field -> Bool
isValid =
    List.filter (\( field, value ) -> isInvalidStatus value)
        >> List.isEmpty


isInvalid : ValidationErrors field -> Bool
isInvalid =
    List.filter (\( field, value ) -> isInvalidStatus value) >> List.isEmpty >> not


isValidField : field -> ValidationErrors field -> Bool
isValidField fieldToCheck errors =
    not (isInvalidField fieldToCheck errors)


isInvalidField : field -> ValidationErrors field -> Bool
isInvalidField fieldToCheck errors =
    errors
        |> List.any (\( field, _ ) -> field == fieldToCheck)


findErrorMessage : field -> ValidationErrors field -> Maybe String
findErrorMessage field errors =
    errors
        |> List.filter (\( f, msg ) -> f == field)
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.andThen getErrorMessage


filter : List field -> ValidationErrors field -> ValidationErrors field
filter fields validations =
    List.filter (\( f, msg ) -> not <| List.any ((==) f) fields) validations


validateDependentStringField : (model -> Bool) -> String -> field -> (model -> String) -> model -> ValidationErrors field
validateDependentStringField dependency error field getter model =
    if dependency model then
        validateField [ getter >> ifBlank ( field, Invalid error ) ] model

    else
        validateField [] model


validateDependentMaybeField : (model -> Bool) -> String -> field -> (model -> Maybe a) -> model -> ValidationErrors field
validateDependentMaybeField dependency error field getter model =
    if dependency model then
        validateMaybeField error field getter model

    else
        validateField [] model


validateStringField : String -> field -> (model -> String) -> model -> ValidationErrors field
validateStringField error field getter =
    validateField [ getter >> ifBlank ( field, Invalid error ) ]


validateBoolField : String -> field -> (model -> Bool) -> model -> ValidationErrors field
validateBoolField error field getter model =
    validateField [ ifInvalid (getter >> not) ( field, Invalid error ) ] model


validateMaybeField : String -> field -> (model -> Maybe a) -> model -> ValidationErrors field
validateMaybeField error field getter =
    validateField [ getter >> ifNothing ( field, Invalid error ) ]


validateMaybeStringField : String -> field -> (model -> Maybe String) -> model -> ValidationErrors field
validateMaybeStringField error field getter model =
    validateField
        [ getter
            >> Validate.all
                [ Validate.ifNothing ( field, Invalid error )
                , Maybe.map (Validate.ifBlank ( field, Invalid error )) >> Maybe.withDefault []
                ]
        ]
        model
        |> (\validations ->
                if List.isEmpty validations then
                    [ ( field, Valid ) ]

                else
                    validations
           )


validateListNotEmptyField : String -> field -> (model -> List a) -> model -> ValidationErrors field
validateListNotEmptyField error field getter =
    validateField [ getter >> Validate.ifInvalid List.isEmpty ( field, Invalid error ) ]


validateField : List (Validate.Validator ( field, ValidationStatus ) model) -> model -> ValidationErrors field
validateField validators model =
    Validate.all validators model


merge : (( field, ValidationStatus ) -> comparable) -> ValidationErrors field -> ValidationErrors field -> ValidationErrors field
merge toComparable first second =
    (first ++ second)
        |> Dict.Extra.groupBy toComparable
        |> Dict.map (always keepValids)
        |> Dict.values
        |> List.concat


keepValids : List ( field, ValidationStatus ) -> List ( field, ValidationStatus )
keepValids validations =
    validations
        |> List.filter (Tuple.second >> isValidStatus)
        |> (\valids ->
                if List.isEmpty valids then
                    toSingleton validations

                else
                    toSingleton valids
           )


toSingleton : List ( field, ValidationStatus ) -> List ( field, ValidationStatus )
toSingleton validations =
    validations |> List.head |> Maybe.map List.singleton |> Maybe.withDefault validations


localize : field -> String
localize field =
    toString field ++ ".Required"



-- updateValidations : field -> ValidationErrors field -> ValidationErrors field -> ValidationErrors field
-- updateValidations field validations currentValidations =
--     currentValidations
--         |> List.filter (\( valField, _ ) -> field /= valField)
--         |> (++) validations
