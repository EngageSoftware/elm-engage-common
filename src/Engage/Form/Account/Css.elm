module Engage.Form.Account.Css exposing
    ( Class(..)
    , css
    )

import Css exposing (..)
import Css.Namespace
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


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    namespace
        |> Namespace.toString
        |> (\ns -> Css.Namespace.namespace ns (snippets theme))
        |> stylesheet


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


accountMixin : Theme -> Mixin
accountMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        , position relative
        , displayFlex
        , flexDirection column
        ]


largeAccountMixin : Theme -> Mixin
largeAccountMixin theme =
    mixin [ flexDirection row ]


accountHeaderMixin : Theme -> Mixin
accountHeaderMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        , displayFlex
        , flexDirection column
        , marginBottom (em 1)
        ]
