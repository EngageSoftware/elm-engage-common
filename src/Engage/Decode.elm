module Engage.Decode exposing (isoDateDecoder)

{-| Engage.Decode

@docs isoDateDecoder

-}

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder)


{-| ISO8601 Date Decoder

For example:
2020-05-22
2025-05-27T10:21:00.607

-}
isoDateDecoder : Decoder Date
isoDateDecoder =
    let
        resultToDecoder : Result String a -> Decoder a
        resultToDecoder result =
            case result of
                Ok value ->
                    Decode.succeed value

                Err error ->
                    Decode.fail error
    in
    Decode.string
        |> Decode.map (String.slice 0 10)
        |> Decode.map Date.fromIsoString
        |> Decode.andThen resultToDecoder
