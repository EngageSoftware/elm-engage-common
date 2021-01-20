module Engage.Html.Extra exposing (domLoadNotifier, none, stylesheet)

{-| Html.Extra

@docs domLoadNotifier, none, stylesheet

-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode


{-| DEPRECATED: To be removed in 9.0.0
-}
none : Html.Html msg
none =
    Html.text ""


{-| DEPRECATED: To be removed in 9.0.0
-}
stylesheet : String -> Html.Html msg
stylesheet href =
    let
        tag =
            "link"

        attrs =
            [ Html.Attributes.attribute "rel" "stylesheet"
            , Html.Attributes.attribute "property" "stylesheet"
            , Html.Attributes.attribute "href" href
            ]

        children =
            []
    in
    Html.node "link" attrs children


{-| Get a dom load notifier
-}
domLoadNotifier : msg -> Html msg
domLoadNotifier msg =
    Html.img
        [ Html.Attributes.src "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"
        , Html.Attributes.style "display" "none"
        , Html.Events.on "load" (Json.Decode.succeed msg)
        ]
        []
