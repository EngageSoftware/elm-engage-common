module Engage.Entity.PhoneNumber exposing
    ( PhoneNumber
    , decoder
    , defaultIsoCode
    , empty
    , encoder
    , format
    )

import Dict
import Engage.String exposing (space)
import IntlPhoneInput.Config
import IntlPhoneInput.Type
import Json.Decode exposing (Decoder, null, oneOf, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode
import String


type alias PhoneNumber =
    IntlPhoneInput.Type.PhoneNumber


empty : PhoneNumber
empty =
    { isoCode = "US", phoneNumber = "" }


isEmpty : PhoneNumber -> Bool
isEmpty phoneNumber =
    phoneNumber.phoneNumber
        |> String.trim
        |> String.isEmpty


defaultIsoCode : PhoneNumber -> PhoneNumber
defaultIsoCode phoneNumber =
    if String.isEmpty phoneNumber.isoCode then
        { phoneNumber | isoCode = "US" }

    else
        phoneNumber


decoder : Decoder PhoneNumber
decoder =
    decode (\isoCode phoneNumber -> { isoCode = isoCode, phoneNumber = phoneNumber } |> defaultIsoCode)
        |> required "isoCode" (oneOf [ null "", string ])
        |> required "phoneNumber" (oneOf [ null "", string ])


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


format : IntlPhoneInput.Config.Config msg -> PhoneNumber -> String
format config phoneNumber =
    let
        dialCode =
            case Dict.get (String.toUpper phoneNumber.isoCode) config.countries |> Maybe.map .dialCode of
                Just code ->
                    "+" ++ code

                Nothing ->
                    ""
    in
    if isEmpty phoneNumber then
        ""

    else
        dialCode |> space phoneNumber.phoneNumber |> String.trim
