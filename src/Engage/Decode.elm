module Engage.Decode exposing (..)

{-| Engage.Decode

@docs isoDateDecoder

-}

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder)


{-| ISO8601 Date Decoder

For example:
2020-05-22

-}
isoDateDecoder : Decoder Date
isoDateDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case Date.fromIsoString str of
                    Ok date ->
                        Decode.succeed date

                    Err error ->
                        Decode.fail error
            )
