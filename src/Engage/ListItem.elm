module Engage.ListItem exposing
    ( ListItem
    , decoder, fromDropdownItem
    )

{-| ListItem

@docs ListItem

@docs decoder, fromDropdownItem

-}

import Json.Decode exposing (Decoder, int, string, succeed)
import Json.Decode.Pipeline exposing (required)
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
        Just parsedId ->
            Just ( parsedId, value )

        Nothing ->
            Nothing


{-| A ListItem decoder
-}
decoder : Decoder ( Int, String )
decoder =
    Json.Decode.oneOf
        [ succeed (\a b -> ( a, b ))
            |> required "key" int
            |> required "value" string
        , succeed (\a b -> ( a, b ))
            |> required "key" (string |> Json.Decode.andThen (String.toInt >> fromResult))
            |> required "value" string
        ]


fromResult : Maybe a -> Decoder a
fromResult result =
    case result of
        Just a ->
            Json.Decode.succeed a

        Nothing ->
            Json.Decode.fail ""
