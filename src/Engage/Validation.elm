module Engage.Validation exposing
    ( RemoteValidationErrors, ValidationErrors, ValidationStatus(..)
    , validateBoolField, validateDependentMaybeField, validateDependentStringField, validateField, validateListNotEmptyField, validateMaybeField, validateMaybeStringField, validateStringField, validateEmailField
    , getErrors, fieldError, getFields, filter, findErrorMessage, isInvalid, isInvalidField, isValid, isValidField, localizeRequired, merge, toError
    )

{-| Validation

@docs RemoteValidationErrors, ValidationErrors, ValidationStatus

@docs validateBoolField, validateDependentMaybeField, validateDependentStringField, validateField, validateListNotEmptyField, validateMaybeField, validateMaybeStringField, validateStringField, validateEmailField

@docs getErrors, fieldError, getFields, filter, findErrorMessage, isInvalid, isInvalidField, isValid, isValidField, localizeRequired, merge, toError

-}

import Dict
import Dict.Extra
import Engage.Localization as Localization exposing (Localization)
import Engage.UI.Error as Error exposing (Status)
import Maybe.Extra
import RemoteData exposing (WebData)
import String.Extra
import Validate exposing (ifBlank, ifNothing)


{-| A ValidationStatus type
-}
type ValidationStatus
    = Valid
    | Invalid String
    | Ignored


{-| A ValidationErrors type
-}
type alias ValidationErrors field =
    List ( field, ValidationStatus )


{-| A RemoteValidationErrors type
-}
type alias RemoteValidationErrors =
    WebData (List String)


{-| Convert a ValidationErrors to a Status
-}
toError : ValidationErrors a -> Status
toError validations =
    case getErrors validations of
        [] ->
            Error.None { infos = [] }

        reasons ->
            Error.Error { reasons = reasons }


{-| Get a field error Status
-}
fieldError : Localization -> field -> ValidationErrors field -> Status
fieldError localization field validations =
    case findErrorMessage field validations of
        Nothing ->
            Error.None { infos = [] }

        Just error ->
            Error.Error { reasons = [ Localization.localizeStringWithDefault error error { localization = localization } ] }


{-| Convert ValidationErrors to a List
-}
getErrors : ValidationErrors field -> List String
getErrors validations =
    validations
        |> List.map Tuple.second
        |> List.filterMap getErrorMessage


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


{-| Get the fields from the ValidationErrors
-}
getFields : ValidationErrors field -> List field
getFields =
    List.map Tuple.first


{-| Check if a ValidationErrors is valid
-}
isValid : ValidationErrors field -> Bool
isValid =
    List.filter (\( _, value ) -> isInvalidStatus value)
        >> List.isEmpty


{-| Check if a ValidationErrors is invalid
-}
isInvalid : ValidationErrors field -> Bool
isInvalid =
    List.filter (\( _, value ) -> isInvalidStatus value) >> List.isEmpty >> not


{-| Check if a ValidationErrors field is valid
-}
isValidField : field -> ValidationErrors field -> Bool
isValidField fieldToCheck errors =
    not (isInvalidField fieldToCheck errors)


{-| Check if a ValidationErrors field is invalid
-}
isInvalidField : field -> ValidationErrors field -> Bool
isInvalidField fieldToCheck errors =
    errors
        |> List.any (\( field, _ ) -> field == fieldToCheck)


{-| Find an error message for a field
-}
findErrorMessage : field -> ValidationErrors field -> Maybe String
findErrorMessage field errors =
    errors
        |> List.filter (\( f, _ ) -> f == field)
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.andThen getErrorMessage


{-| Filter ValidationErrors using a List of fields
-}
filter : List field -> ValidationErrors field -> ValidationErrors field
filter fields validations =
    List.filter (\( f, _ ) -> not <| List.any ((==) f) fields) validations


{-| Validate a dependent String field
-}
validateDependentStringField : (model -> Bool) -> String -> field -> (model -> String) -> Validate.Validator ( field, ValidationStatus ) model
validateDependentStringField dependency error field getter =
    Validate.ifTrue
        (\model ->
            if dependency model then
                String.Extra.isBlank (getter model)

            else
                False
        )
        ( field, Invalid error )


{-| Validate a dependent Maybe field
-}
validateDependentMaybeField : (model -> Bool) -> String -> field -> (model -> Maybe a) -> Validate.Validator ( field, ValidationStatus ) model
validateDependentMaybeField dependency error field getter =
    Validate.ifTrue
        (\model ->
            if dependency model then
                Maybe.Extra.isNothing (getter model)

            else
                False
        )
        ( field, Invalid error )


{-| Validate a String field
-}
validateStringField : String -> field -> (model -> String) -> Validate.Validator ( field, ValidationStatus ) model
validateStringField error field getter =
    ifBlank getter ( field, Invalid error )


{-| Validate a Bool field
-}
validateBoolField : String -> field -> (model -> Bool) -> Validate.Validator ( field, ValidationStatus ) model
validateBoolField error field getter =
    Validate.ifTrue (getter >> not) ( field, Invalid error )


{-| Validate a Maybe field
-}
validateMaybeField : String -> field -> (model -> Maybe a) -> Validate.Validator ( field, ValidationStatus ) model
validateMaybeField error field getter =
    ifNothing getter ( field, Invalid error )


{-| Validate a Maybe String field
-}
validateMaybeStringField : String -> field -> (model -> Maybe String) -> Validate.Validator ( field, ValidationStatus ) model
validateMaybeStringField error field getter =
    Validate.all
        [ Validate.ifNothing getter ( field, Invalid error )
        , Validate.ifBlank (getter >> Maybe.withDefault "") ( field, Invalid error )
        ]


{-| Validate a List not empty field
-}
validateListNotEmptyField : String -> field -> (model -> List a) -> Validate.Validator ( field, ValidationStatus ) model
validateListNotEmptyField error field getter =
    Validate.ifTrue (getter >> List.isEmpty) ( field, Invalid error )


{-| Validate an email field
-}
validateEmailField : String -> field -> (model -> String) -> Validate.Validator ( field, ValidationStatus ) model
validateEmailField error field getter =
    Validate.ifInvalidEmail getter (\_ -> ( field, Invalid error ))


{-| Validate a field
-}
validateField : List (Validate.Validator ( field, ValidationStatus ) model) -> model -> ValidationErrors field
validateField validators model =
    getErrorsFromResult (Validate.validate (Validate.all validators) model)


{-| Merge two ValidationErrors
-}
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


{-| Localize a field to a String
-}
localizeRequired : String -> String
localizeRequired field =
    field ++ ".Required"



-- updateValidations : field -> ValidationErrors field -> ValidationErrors field -> ValidationErrors field
-- updateValidations field validations currentValidations =
--     currentValidations
--         |> List.filter (\( valField, _ ) -> field /= valField)
--         |> (++) validations


getErrorsFromResult : Result (List ( field, ValidationStatus )) (Validate.Valid subject) -> ValidationErrors field
getErrorsFromResult validations =
    case validations of
        Err errors ->
            errors

        Ok _ ->
            []
