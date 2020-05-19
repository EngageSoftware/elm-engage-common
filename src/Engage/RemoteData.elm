module Engage.RemoteData exposing
    ( RemoteData(..)
    , WebData
    , downgrade
    , fail
    , isLoading
    , isSuccess
    , loading
    , map
    , mapError
    , toError
    , toMaybe
    , upgrade
    )

{-| RemoteData

@docs RemoteData, WebData

@docs downgrade, fail, isLoading, isSuccess, loading, map, mapError, toError, toMaybe, upgrade

-}

import Http
import RemoteData


{-| The RemoteData type
-}
type RemoteData e a
    = NotAsked
    | Failure e
    | Loading
    | Reloading a
    | FailureWithData e a
    | Success a


{-| The WebData type
-}
type alias WebData a =
    RemoteData Http.Error a


{-| Get a loading RemoteData
-}
loading : RemoteData e a -> RemoteData e a
loading remoteData =
    remoteData
        |> toMaybe
        |> Maybe.map (\data -> Reloading data)
        |> Maybe.withDefault Loading


{-| Fail a RemoteData
-}
fail : e -> RemoteData e a -> RemoteData e a
fail error remoteData =
    remoteData
        |> toMaybe
        |> Maybe.map (\data -> FailureWithData error data)
        |> Maybe.withDefault (Failure error)


{-| Map the error of RemoteData
-}
mapError : (e -> ee) -> RemoteData e a -> RemoteData ee a
mapError f remoteData =
    case remoteData of
        NotAsked ->
            NotAsked

        Failure e ->
            Failure (f e)

        FailureWithData e a ->
            FailureWithData (f e) a

        Loading ->
            Loading

        Reloading a ->
            Reloading a

        Success a ->
            Success a


{-| Get the error of RemoteData
-}
toError : RemoteData e a -> Maybe e
toError remoteData =
    case remoteData of
        NotAsked ->
            Nothing

        Failure e ->
            Just e

        FailureWithData e a ->
            Just e

        Loading ->
            Nothing

        Reloading a ->
            Nothing

        Success a ->
            Nothing


{-| Get the Maybe value of RemoteData
-}
toMaybe : RemoteData e a -> Maybe a
toMaybe remoteData =
    case remoteData of
        NotAsked ->
            Nothing

        Failure _ ->
            Nothing

        FailureWithData _ a ->
            Just a

        Loading ->
            Nothing

        Reloading a ->
            Just a

        Success a ->
            Just a


{-| Check if the RemoteData is loading
-}
isLoading : RemoteData e a -> Bool
isLoading remoteData =
    case remoteData of
        NotAsked ->
            False

        Failure _ ->
            False

        FailureWithData _ _ ->
            False

        Success _ ->
            False

        Loading ->
            True

        Reloading _ ->
            True


{-| Map onto the RemoteData
-}
map : (a -> b) -> RemoteData e a -> RemoteData e b
map f remoteData =
    case remoteData of
        NotAsked ->
            NotAsked

        Failure e ->
            Failure e

        Loading ->
            Loading

        FailureWithData e a ->
            FailureWithData e (f a)

        Reloading a ->
            Reloading (f a)

        Success a ->
            Success (f a)


{-| Upgrade the RemoteData
-}
upgrade : RemoteData.RemoteData e a -> RemoteData e a
upgrade remoteData =
    case remoteData of
        RemoteData.NotAsked ->
            NotAsked

        RemoteData.Failure e ->
            Failure e

        RemoteData.Loading ->
            Loading

        RemoteData.Success a ->
            Success a


{-| Downgrade the RemoteData

    Use this with caution, you lose some information with this
-}
downgrade : RemoteData e a -> RemoteData.RemoteData e a
downgrade remoteData =
    case remoteData of
        NotAsked ->
            RemoteData.NotAsked

        Failure e ->
            RemoteData.Failure e

        FailureWithData e a ->
            RemoteData.Failure e

        Loading ->
            RemoteData.Loading

        Reloading a ->
            RemoteData.Loading

        Success a ->
            RemoteData.Success a


{-| Check if the RemoteData is a success
-}
isSuccess : RemoteData e a -> Bool
isSuccess =
    downgrade >> RemoteData.isSuccess
