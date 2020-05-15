module Engage.ListItem exposing
    ( ListItem
    , decoder
    , fromDropdownItem
    )

import Json.Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (decode, required)
import String


type alias ListItem =
    ( Int, String )


fromDropdownItem : ( String, String ) -> Maybe ListItem
fromDropdownItem ( id, value ) =
    case String.toInt id of
        Ok parsedId ->
            Just ( parsedId, value )

        Err _ ->
            Nothing


decoder : Decoder ( Int, String )
decoder =
    Json.Decode.oneOf
        [ decode (,)
            |> required "key" int
            |> required "value" string
        , decode (,)
            |> required "key" (string |> Json.Decode.andThen (String.toInt >> fromResult))
            |> required "value" string
        ]


fromResult : Result String a -> Decoder a
fromResult result =
    case result of
        Ok a ->
            Json.Decode.succeed a

        Err err ->
            Json.Decode.fail err
