module Engage.Form.Password exposing (PasswordSettings, passwordSettingsDecoder, validatePasswordField)

{-| Form.Password

@docs PasswordSettings, passwordSettingsDecoder, validatePasswordField

-}

import Engage.Validation exposing (ValidationStatus(..))
import Json.Decode exposing (Decoder, bool, int, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Regex
import Validate


{-| The password settings type
-}
type alias PasswordSettings =
    { minLength : Int
    , minNonAlphaNumCharacters : Int
    , validationRegex : String
    , showStrengthBar : Bool
    }


{-| The password settings decoder
-}
passwordSettingsDecoder : Decoder PasswordSettings
passwordSettingsDecoder =
    succeed PasswordSettings
        |> required "minLength" int
        |> required "minNonAlphaNumCharacters" int
        |> required "validationRegex" string
        |> required "showStrengthBar" bool


{-| Validate a password field
-}
validatePasswordField : PasswordSettings -> String -> field -> (model -> String) -> Validate.Validator ( field, ValidationStatus ) model
validatePasswordField passwordSettings error field getter =
    let
        nonAlphaNumCount =
            String.foldl
                (\char count ->
                    count
                        + (if Char.isAlphaNum char then
                            1

                           else
                            0
                          )
                )
                0

        regex =
            Maybe.withDefault Regex.never <|
                Regex.fromString passwordSettings.validationRegex

        checkRegex =
            Regex.contains regex

        -- \value -> String.isEmpty passwordSettings.validationRegex || Regex.contains regex value
    in
    Validate.all
        [ Validate.ifFalse (getter >> String.length >> (<=) passwordSettings.minLength) ( field, Invalid error )
        , Validate.ifFalse (getter >> nonAlphaNumCount >> (<=) passwordSettings.minNonAlphaNumCharacters) ( field, Invalid error )
        , Validate.ifFalse (getter >> checkRegex) ( field, Invalid error )
        ]
