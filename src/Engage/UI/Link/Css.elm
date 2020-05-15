module Engage.UI.Link.Css exposing (Class(..), css, snippets)

import Css exposing (..)
import Css.Namespace
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Importance(..))
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)
import Engage.ThemeHelper as Theme


type Class
    = Link Importance


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class (Link Standard)
        [ linkMixin theme
        , standardMixin theme
        ]
    ]


linkMixin : Theme -> Mixin
linkMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        ]



-- IMPORTANCE


standardMixin : Theme -> Mixin
standardMixin theme =
    let
        palette =
            Theme.palette theme
    in
    mixin
        []
