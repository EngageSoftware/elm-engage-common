module Engage.CreditCard.Ports exposing
    ( InKey(..)
    , OutKey(..)
    , inKeyDecoder
    , toInKey
    )

import Json.Decode exposing (..)


inKeyDecoder : Decoder InKey
inKeyDecoder =
    string |> andThen toInKey


toInKey : String -> Decoder InKey
toInKey str =
    case str of
        "StripeCardError" ->
            succeed StripeCardError

        "StripeCardReady" ->
            succeed StripeCardReady

        "GetStripeTokenCompleted" ->
            succeed GetStripeTokenCompleted

        "GeneralError" ->
            succeed GeneralError

        key ->
            fail <| "Unknown port in key: " ++ key


type OutKey
    = DomReady
    | GetStripeToken


type InKey
    = StripeCardError
    | StripeCardReady
    | GetStripeTokenCompleted
    | GeneralError
