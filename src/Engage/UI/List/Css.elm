module Engage.UI.List.Css exposing
    ( Class(..)
    , css
    , snippets
    )

import Css exposing (..)
import Css.Namespace
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)


type Class
    = List
    | ListItem


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
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
