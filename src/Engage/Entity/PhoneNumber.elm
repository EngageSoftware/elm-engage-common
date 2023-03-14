module Engage.Entity.PhoneNumber exposing
    ( PhoneNumber
    , decoder, defaultIsoCode, empty, encoder, format
    )

{-| Entity.PhoneNumber

@docs PhoneNumber

@docs decoder, defaultIsoCode, empty, encoder, format

-}

import Dict
import Engage.String exposing (space)
import IntlPhoneInput.Config
import IntlPhoneInput.Type
import Json.Decode exposing (Decoder, null, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import String


{-| The PhoneNumber type
-}
type alias PhoneNumber =
    IntlPhoneInput.Type.PhoneNumber


{-| Get an empty phone number
-}
empty : PhoneNumber
empty =
    { isoCode = "US", phoneNumber = "" }


isEmpty : PhoneNumber -> Bool
isEmpty phoneNumber =
    phoneNumber.phoneNumber
        |> String.trim
        |> String.isEmpty


{-| Get the default iso code
-}
defaultIsoCode : PhoneNumber -> PhoneNumber
defaultIsoCode phoneNumber =
    if String.isEmpty phoneNumber.isoCode then
        { phoneNumber | isoCode = "US" }

    else
        phoneNumber


{-| The PhoneNumber decoder
-}
decoder : Decoder PhoneNumber
decoder =
    succeed (\isoCode phoneNumber -> { isoCode = isoCode, phoneNumber = phoneNumber } |> defaultIsoCode)
        |> required "isoCode" (oneOf [ null "", string ])
        |> required "phoneNumber" (oneOf [ null "", string ])


{-| The PhoneNumber encoder
-}
encoder : PhoneNumber -> Json.Encode.Value
encoder phoneNumber =
    if isEmpty phoneNumber then
        Json.Encode.object
            [ ( "phoneNumber", Json.Encode.string "" )
            , ( "isoCode", Json.Encode.string "" )
            ]

    else
        Json.Encode.object
            [ ( "phoneNumber", Json.Encode.string phoneNumber.phoneNumber )
            , ( "isoCode", Json.Encode.string phoneNumber.isoCode )
            ]


{-| Format a phone number
-}
format : IntlPhoneInput.Config.Config msg -> PhoneNumber -> String
format config phoneNumber =
    let
        dialCode =
            case Dict.get (String.toUpper phoneNumber.isoCode) config.countries |> Maybe.map .dialCode of
                Just code ->
                    if code /= "1" then
                        "+" ++ code
                    else
                        ""

                Nothing ->
                    ""
        number = 
            space (formatAmericanPhone phoneNumber.phoneNumber)
    in
    if isEmpty phoneNumber then
        ""

    else
        dialCode |> number |> String.trim


formatAmericanPhone : String -> String
formatAmericanPhone phoneNumber = 
    if String.length phoneNumber == 10 then
        String.join "-" [String.left 3 phoneNumber, String.slice 3 6 phoneNumber, String.slice 6 10 phoneNumber]

    else
        phoneNumber