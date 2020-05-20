module Engage.Validation exposing
    ( RemoteValidationErrors, ValidationResult, ValidationStatus(..)
    , getErrors, fieldError, fields, filter, findErrorMessage, isInvalidField, isValidField, localize, merge, toError, validateBoolField, validateDependentMaybeField, validateDependentStringField, validateField, validateListNotEmptyField, validateMaybeField, validateMaybeStringField, validateStringField
    )

{-| Validation

@docs RemoteValidationErrors, ValidationResult, ValidationStatus

@docs getErrors, fieldError, fields, filter, findErrorMessage, isInvalidField, isValidField, localize, merge, toError, validateBoolField, validateDependentMaybeField, validateDependentStringField, validateField, validateListNotEmptyField, validateMaybeField, validateMaybeStringField, validateStringField

-}

import Dict
import Dict.Extra
import Engage.Localization as Localization exposing (Localization)
import Engage.UI.Error as Error exposing (Status)
import RemoteData exposing (WebData)
import Validate exposing (ifBlank, ifNothing, ifTrue)


{-| A ValidationStatus type
-}
type ValidationStatus
    = Valid
    | Invalid String
    | Ignored


{-| A ValidationErrors type
-}
type alias ValidationResult field model =
    Result (List ( field, ValidationStatus )) (Validate.Valid model)


{-| A RemoteValidationErrors type
-}
type alias RemoteValidationErrors =
    WebData (List String)


{-| Convert a ValidationErrors to a Status
-}
toError : ValidationResult a m -> Status
toError validations =
    case getErrors validations of
        [] ->
            Error.None { infos = [] }

        reasons ->
            Error.Error { reasons = reasons }


{-| Get a field error Status
-}
fieldError : Localization -> field -> ValidationResult field model -> Status
fieldError localization field validations =
    case findErrorMessage field validations of
        Nothing ->
            Error.None { infos = [] }

        Just error ->
            Error.Error { reasons = [ Localization.localizeStringWithDefault error error { localization = localization } ] }


{-| Convert ValidationErrors to a List
-}
getErrors : ValidationResult field model -> List String
getErrors validations =
    case validations of
        Err errors ->
            errors
                |> List.map Tuple.second
                |> List.filterMap getErrorMessage

        Ok _ ->
            []


{-| Check if a ValidationStatus is valid
-}
isValidStatus : ValidationStatus -> Bool
isValidStatus validationStatus =
    case validationStatus of
        Valid ->
            True

        _ ->
            False


{-| Check if a ValidationStatus is invalid
-}
isInvalidStatus : ValidationStatus -> Bool
isInvalidStatus validationStatus =
    case validationStatus of
        Valid ->
            False

        Invalid _ ->
            True

        Ignored ->
            False


{-| Get the error message from ValidationStatus
-}
getErrorMessage : ValidationStatus -> Maybe String
getErrorMessage validationStatus =
    case validationStatus of
        Invalid msg ->
            Just msg

        Valid ->
            Nothing

        Ignored ->
            Nothing


{-| Get the fields from the ValidationResult
-}
fields : ValidationResult field model -> List field
fields validations =
    case validations of
        Err errors ->
            errors
                |> List.map Tuple.first

        Ok _ ->
            []


{-| Check if a ValidationResult field is valid
-}
isValidField : field -> ValidationResult field model -> Bool
isValidField fieldToCheck errors =
    not (isInvalidField fieldToCheck errors)


{-| Check if a ValidationResult field is invalid
-}
isInvalidField : field -> ValidationResult field model -> Bool
isInvalidField fieldToCheck validations =
    case validations of
        Err errors ->
            errors
                |> List.any (\( field, _ ) -> field == fieldToCheck)

        Ok _ ->
            False


{-| Find an error message for a field
-}
findErrorMessage : field -> ValidationResult field model -> Maybe String
findErrorMessage field validations =
    case validations of
        Err errors ->
            errors
                |> List.filter (\( f, msg ) -> f == field)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.andThen getErrorMessage

        Ok _ ->
            Nothing


{-| Filter ValidationResult using a List of fields
-}
filter : List field -> ValidationResult field model -> ValidationResult field model
filter fieldList validations =
    validations
        |> Result.mapError
            (List.filter (\( f, msg ) -> not <| List.any ((==) f) fieldList))


{-| Validate a dependent String field
-}
validateDependentStringField : (model -> Bool) -> String -> field -> (model -> String) -> model -> ValidationResult field model
validateDependentStringField dependency error field getter model =
    if dependency model then
        validateField [ ifBlank getter ( field, Invalid error ) ] model

    else
        validateField [] model


{-| Validate a dependent Maybe field
-}
validateDependentMaybeField : (model -> Bool) -> String -> field -> (model -> Maybe a) -> model -> ValidationResult field model
validateDependentMaybeField dependency error field getter model =
    if dependency model then
        validateMaybeField error field getter model

    else
        validateField [] model


{-| Validate a String field
-}
validateStringField : String -> field -> (model -> String) -> model -> ValidationResult field model
validateStringField error field getter =
    validateField [ ifBlank getter ( field, Invalid error ) ]


{-| Validate a Bool field
-}
validateBoolField : String -> field -> (model -> Bool) -> model -> ValidationResult field model
validateBoolField error field getter model =
    validateField [ Validate.ifTrue (getter >> not) ( field, Invalid error ) ] model


{-| Validate a Maybe field
-}
validateMaybeField : String -> field -> (model -> Maybe a) -> model -> ValidationResult field model
validateMaybeField error field getter model =
    validateField [ ifNothing getter ( field, Invalid error ) ] model


{-| Validate a Maybe String field
-}
validateMaybeStringField : String -> field -> (model -> Maybe String) -> model -> ValidationResult field model
validateMaybeStringField error field getter model =
    validateField
        [ Validate.all
            [ Validate.ifNothing getter ( field, Invalid error )
            , Validate.ifBlank (getter >> Maybe.withDefault "") ( field, Invalid error )
            ]
        ]
        model
        |> (\validations ->
                validations
                    |> Result.mapError
                        (\errors ->
                            if List.isEmpty errors then
                                [ ( field, Valid ) ]

                            else
                                errors
                        )
           )


{-| Validate a List not empty field
-}
validateListNotEmptyField : String -> field -> (model -> List a) -> model -> ValidationResult field model
validateListNotEmptyField error field getter model =
    validateField [ Validate.ifTrue (getter >> List.isEmpty) ( field, Invalid error ) ] model


{-| Validate a field
-}
validateField : List (Validate.Validator ( field, ValidationStatus ) model) -> model -> ValidationResult field model
validateField validators getter =
    Validate.validate (Validate.all validators) getter


{-| Merge two ValidationResult errors
-}
merge : (( field, ValidationStatus ) -> comparable) -> ValidationResult field model -> ValidationResult field model -> ValidationResult field model
merge toComparable firstResult secondResult =
    firstResult
        |> Result.mapError
            (\first ->
                case secondResult of
                    Err second ->
                        (first ++ second)
                            |> Dict.Extra.groupBy toComparable
                            |> Dict.map (always keepValids)
                            |> Dict.values
                            |> List.concat

                    Ok _ ->
                        first
            )


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


{-| Localize a field to a String
-}
localize : String -> String
localize field =
    field ++ ".Required"



-- updateValidations : field -> ValidationResult field -> ValidationResult field -> ValidationResult field
-- updateValidations field validations currentValidations =
--     currentValidations
--         |> List.filter (\( valField, _ ) -> field /= valField)
--         |> (++) validations
