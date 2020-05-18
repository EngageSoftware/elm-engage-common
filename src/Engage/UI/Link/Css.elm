module Engage.UI.Link.Css exposing (Class(..), css, snippets)

import Css exposing (..)
import Css.Foreign exposing (Snippet, class)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Importance(..))
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)
import Engage.ThemeHelper as Theme


type Class
    = Link Importance


css : Namespace -> Theme -> DEPRECATED.Css.File.Stylesheet
css namespace theme =
    (DEPRECATED.Css.File.stylesheet << DEPRECATED.Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class (Link Standard)
        [ linkMixin theme
        , standardMixin theme
        ]
    ]


linkMixin : Theme -> Style
linkMixin theme =
    batch
        [ BaseCss.normalizeMixin
        ]



-- IMPORTANCE


standardMixin : Theme -> Style
standardMixin theme =
    let
        palette =
            Theme.palette theme
    in
    batch
        []
