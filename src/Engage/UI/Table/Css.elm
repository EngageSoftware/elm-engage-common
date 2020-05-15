module Engage.UI.Table.Css exposing
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
    = Table
    | HeaderRow
    | HeaderCell
    | DataRow
    | DataCell


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class Table
        [ BaseCss.normalizeMixin
        , paddingRight (Css.rem 0.5)
        , descendants
            [ class HeaderRow []
            , class HeaderCell []
            , class DataRow []
            , class DataCell []
            ]
        ]
    ]
