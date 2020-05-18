port module Engage.Ports exposing
    ( SharedOutKey(..)
    , decoder
    , encoder
    , out
    , send
    , sub
    )

{-| Ports helpers

@docs SharedOutKey

@docs decoder, encoder, out, send, sub

-}

import Engage.Namespace as Namespace
import Json.Decode exposing (..)
import Json.Encode


{-| A standard out port
-}
port out : Json.Encode.Value -> Cmd msg


{-| A subscription port
-}
port sub : (Json.Decode.Value -> msg) -> Sub msg


engagecoreNamespace : String
engagecoreNamespace =
    Namespace.toString Namespace.engagecore


{-| Send a value based on a key
-}
send : outKey -> Json.Encode.Value -> Cmd msg
send key value =
    encoder key value |> out


{-| Encode a value based on a key
-}
encoder : outKey -> Json.Encode.Value -> Json.Encode.Value
encoder key value =
    Json.Encode.object
        [ ( engagecoreNamespace ++ "Key", Json.Encode.string (toString key) )
        , ( "value", value )
        ]


{-| Decode a value based on a key
-}
decoder : Decoder inKey -> (inKey -> Decoder a) -> Decoder a
decoder inKeyDecoder valueDecoder =
    field (engagecoreNamespace ++ "Key") inKeyDecoder
        |> andThen (\inKey -> field "value" (valueDecoder inKey))


{-| A SharedOutKey type
-}
type SharedOutKey
    = ScrollTo
