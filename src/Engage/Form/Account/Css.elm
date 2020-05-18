module Engage.Form.Account.Css exposing
    ( Class(..)
    , css
    )

import Css exposing (..)
import Css.Foreign exposing (Snippet, class)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Css as BaseCss
import Engage.Styles.MediaQuery as MediaQuery
import Engage.Theme as Theme exposing (Theme)
import Engage.ThemeHelper as Theme


type Class
    = Account
    | AccountHeader
    | AccountInfo
    | AccountBody
    | AccountName
    | AccountPhone
    | AccountEditButton


css : Namespace -> Theme -> DEPRECATED.Css.File.Stylesheet
css namespace theme =
    namespace
        |> Namespace.toString
        |> (\ns -> DEPRECATED.Css.Namespace.namespace ns (snippets theme))
        |> DEPRECATED.Css.File.stylesheet


snippets : Theme -> List Snippet
snippets theme =
    let
        themePalette =
            Theme.palette theme
    in
    [ class Account [ accountMixin theme ]
    , MediaQuery.atMedia MediaQuery.Medium
        [ class Account [ largeAccountMixin theme ] ]
    , class AccountHeader [ accountHeaderMixin theme ]
    , class AccountInfo [ width (pct 100) ]
    ]


accountMixin : Theme -> Style
accountMixin theme =
    batch
        [ BaseCss.normalizeMixin
        , position relative
        , displayFlex
        , flexDirection column
        ]


largeAccountMixin : Theme -> Style
largeAccountMixin theme =
    batch [ flexDirection row ]


accountHeaderMixin : Theme -> Style
accountHeaderMixin theme =
    batch
        [ BaseCss.normalizeMixin
        , displayFlex
        , flexDirection column
        , marginBottom (em 1)
        ]
