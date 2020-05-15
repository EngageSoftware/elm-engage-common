module Engage.RemoteValidation exposing
    ( RemoteValidationErrors
    , httpErrorToValidationResult
    , isValid
    , isValidWebData
    , serverErrorDecoder
    , toValidationResult
    , webDataToError
    )

import Dict exposing (Dict)
import Engage.Http
import Engage.RemoteData exposing (WebData)
import Engage.UI.Error as Error exposing (Status)
import Http
import Json.Decode as Decode exposing (Decoder)
import String


type alias RemoteValidationErrors =
    List String


type ServerError
    = ServerError String
    | ValidationError (List String)


serverErrorDecoder : Decoder ServerError
serverErrorDecoder =
    Decode.oneOf
        [ Decode.field "validationErrors" (Decode.list Decode.string)
            |> Decode.map ValidationError
        , Engage.Http.serverErrorDecoder { localization = Dict.empty }
            |> Decode.map ServerError
        ]


serverErrorToErrors : ServerError -> List String
serverErrorToErrors serverError =
    case serverError of
        ServerError err ->
            if err |> String.trim |> String.isEmpty |> not then
                [ err ]

            else
                []

        ValidationError err ->
            err


webDataToError : WebData data -> Status
webDataToError webData =
    case webData of
        Engage.RemoteData.Failure httpError ->
            httpErrorToValidationResult httpError

        Engage.RemoteData.FailureWithData httpError _ ->
            httpErrorToValidationResult httpError

        Engage.RemoteData.Success _ ->
            Error.None { infos = [] }

        Engage.RemoteData.NotAsked ->
            Error.None { infos = [] }

        Engage.RemoteData.Loading ->
            Error.None { infos = [] }

        Engage.RemoteData.Reloading _ ->
            Error.None { infos = [] }


toValidationResult : RemoteValidationErrors -> Status
toValidationResult validationErrors =
    if List.isEmpty validationErrors then
        Error.None { infos = [] }

    else
        Error.Error { reasons = validationErrors }


httpErrorToValidationResult : Http.Error -> Status
httpErrorToValidationResult error =
    case error of
        Http.BadUrl error ->
            Error.Error { reasons = [ error ] }

        Http.BadStatus response ->
            Error.Error
                { reasons =
                    response.body
                        |> Decode.decodeString serverErrorDecoder
                        |> Result.toMaybe
                        |> Maybe.map serverErrorToErrors
                        |> Maybe.withDefault [ response.status.message ]
                }

        Http.BadPayload error response ->
            Error.Error { reasons = [ error ] }

        Http.Timeout ->
            Error.Error { reasons = [ "Timeout" ] }

        Http.NetworkError ->
            Error.Error { reasons = [ "NetworkError" ] }


isValidWebData : WebData data -> Bool
isValidWebData webData =
    Engage.RemoteData.isSuccess webData


isValid : RemoteValidationErrors -> Bool
isValid =
    List.isEmpty >> not
