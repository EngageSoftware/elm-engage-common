module Engage.UI.Dialog.Css exposing (Class(..), css)

import Css exposing (..)
import Css.Foreign exposing (Snippet, class)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)


type Class
    = Dialog
    | DialogOverlay
    | DialogQuestion
    | DialogAnswer
    | DialogYes
    | DialogNo


css : Namespace -> Theme -> DEPRECATED.Css.File.Stylesheet
css namespace theme =
    (DEPRECATED.Css.File.stylesheet << DEPRECATED.Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class DialogOverlay
        [ BaseCss.normalizeMixin
        , position fixed
        , top zero
        , left zero
        , backgroundColor (rgba 0 0 0 0.75)
        , width (vw 100)
        , height (vh 100)
        , displayFlex
        , flexDirection column
        , justifyContent center
        , alignItems center
        , zIndex (int 2147483647)
        , focus [ outline none ]
        ]
    , class Dialog
        [ BaseCss.normalizeMixin
        , displayFlex
        , flexDirection column
        , backgroundColor (rgba 255 255 255 1)
        , boxShadow5 zero (px 2) (px 4) (px 2) (rgba 0 0 0 0.25)
        , justifyContent stretch
        , alignItems stretch
        ]
    , class DialogQuestion
        [ BaseCss.normalizeMixin
        , padding2 (em 4) (em 6)
        ]
    , class DialogAnswer
        [ BaseCss.normalizeMixin
        ]
    , class DialogYes
        [ BaseCss.normalizeMixin
        ]
    , class DialogNo
        [ BaseCss.normalizeMixin
        , marginRight (em 0.5)
        ]
    ]
