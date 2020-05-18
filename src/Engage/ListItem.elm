module Engage.ListItem exposing
    ( ListItem
    , decoder
    , fromDropdownItem
    )

{-| ListItem

@docs ListItem

@docs decoder, fromDropdownItem

-}

import Json.Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (decode, required)
import String


{-| A ListItem type
-}
type alias ListItem =
    ( Int, String )


{-| Get a ListItem from a dropdown item
-}
fromDropdownItem : ( String, String ) -> Maybe ListItem
fromDropdownItem ( id, value ) =
    case String.toInt id of
        Ok parsedId ->
            Just ( parsedId, value )

        Err _ ->
            Nothing


{-| A ListItem decoder
-}
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
