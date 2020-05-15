module Engage.Styles.Css exposing
    ( css
    , normalizeMixin
    )

import Css exposing (..)
import Css.Namespace
import Engage.Namespace as Namespace exposing (Namespace)


css : Namespace -> Stylesheet
css namespace =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
        []


normalizeMixin : Mixin
normalizeMixin =
    mixin
        [ boxSizing borderBox
        , descendants
            [ everything
                [ boxSizing borderBox
                , before [ boxSizing borderBox ]
                , after [ boxSizing borderBox ]
                ]
            ]
        ]
