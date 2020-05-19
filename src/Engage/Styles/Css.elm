module Engage.Styles.Css exposing
    ( css
    , normalizeMixin
    )

{-| Styles.Css

@docs css, normalizeMixin

-}

import Css exposing (..)
import Css.Foreign exposing (descendants, everything)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Namespace as Namespace exposing (Namespace)


{-| Get the css for the Namespace
-}
css : Namespace -> DEPRECATED.Css.File.Stylesheet
css namespace =
    (DEPRECATED.Css.File.stylesheet << DEPRECATED.Css.Namespace.namespace (Namespace.toString namespace))
        []


{-| Get the normalized styles
-}
normalizeMixin : Style
normalizeMixin =
    batch
        [ boxSizing borderBox
        , descendants
            [ everything
                [ boxSizing borderBox
                , before [ boxSizing borderBox ]
                , after [ boxSizing borderBox ]
                ]
            ]
        ]
