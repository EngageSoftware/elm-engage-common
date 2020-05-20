module Engage.RemoteData.View exposing (errorView)

import Engage.Namespace as Namespace exposing (Namespace)
import Engage.RemoteData as RemoteData exposing (WebData)
import Engage.RemoteValidation exposing (httpErrorToValidationResult)
import Engage.UI.Error
import Html exposing (Html)


errorView : Namespace -> WebData a -> Html msg
errorView namespace webData =
    case webData |> RemoteData.toError of
        Just e ->
            e |> httpErrorToValidationResult |> Engage.UI.Error.error { namespace = namespace }

        Nothing ->
            Html.text ""
