port module Engage.Ports exposing
    ( SharedOutKey(..)
    , decoder
    , encoder
    , out
    , send
    , sub
    )

import Engage.Namespace as Namespace
import Json.Decode exposing (..)
import Json.Encode


port out : Json.Encode.Value -> Cmd msg

port sub : (Json.Decode.Value -> msg) -> Sub msg


engagecoreNamespace : String
engagecoreNamespace =
    Namespace.toString Namespace.engagecore


send : outKey -> Json.Encode.Value -> Cmd msg
send key value =
    encoder key value |> out


encoder : outKey -> Json.Encode.Value -> Json.Encode.Value
encoder key value =
    Json.Encode.object
        [ ( engagecoreNamespace ++ "Key", Json.Encode.string (toString key) )
        , ( "value", value )
        ]


decoder : Decoder inKey -> (inKey -> Decoder a) -> Decoder a
decoder inKeyDecoder valueDecoder =
    field (engagecoreNamespace ++ "Key") inKeyDecoder
        |> andThen (\inKey -> field "value" (valueDecoder inKey))


type SharedOutKey
    = ScrollTo
