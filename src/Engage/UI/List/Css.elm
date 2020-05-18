module Engage.UI.List.Css exposing
    ( Class(..)
    , css
    , snippets
    )

import Css exposing (..)
import Css.Foreign exposing (Snippet, class, descendants)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)


type Class
    = List
    | ListItem


css : Namespace -> Theme -> DEPRECATED.Css.File.Stylesheet
css namespace theme =
    (DEPRECATED.Css.File.stylesheet << DEPRECATED.Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class List
        [ BaseCss.normalizeMixin
        , listStyle none
        , padding (px 0)
        , descendants
            [ class ListItem []
            ]
        ]
    ]
