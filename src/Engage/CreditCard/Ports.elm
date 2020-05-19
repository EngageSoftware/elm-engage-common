module Engage.CreditCard.Ports exposing
    ( InKey(..)
    , OutKey(..)
    , inKeyDecoder
    , toInKey
    )

{-| CreditCard.Ports

@docs InKey, OutKey

@docs inKeyDecoder, toInKey

-}

import Json.Decode exposing (..)


{-| The InKey Decoder
-}
inKeyDecoder : Decoder InKey
inKeyDecoder =
    string |> andThen toInKey


{-| Convert a String to an InKey Decoder
-}
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


{-| The OutKey type
-}
type OutKey
    = DomReady
    | GetStripeToken


{-| The InKey Type
-}
type InKey
    = StripeCardError
    | StripeCardReady
    | GetStripeTokenCompleted
    | GeneralError
