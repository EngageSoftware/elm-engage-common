module Engage.Form.FormAction.Css exposing (Class(..), css)

import Css exposing (..)
import Css.Foreign exposing (Snippet, class, children, everything)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)


type Class
    = FormAction
    | FormActionLeft
    | FormActionRight


css : Namespace -> Theme -> DEPRECATED.Css.File.Stylesheet
css namespace theme =
    (DEPRECATED.Css.File.stylesheet << DEPRECATED.Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class FormAction
        [ BaseCss.normalizeMixin
        , displayFlex
        , flexDirection row
        , justifyContent spaceBetween
        , padding2 (em 1) (em 0.5)
        , borderTop3 (px 1) solid (rgba 0 0 0 0.1)
        ]
    , class FormActionLeft
        [ BaseCss.normalizeMixin
        , formActionMixin
        ]
    , class FormActionRight
        [ BaseCss.normalizeMixin
        , formActionMixin
        ]
    ]


formActionMixin : Style
formActionMixin =
    batch
        [ children [ everything [ marginLeft (em 0.5), marginRight (em 0.5) ] ]
        , displayFlex
        , flexDirection row
        ]
