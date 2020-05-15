module Engage.UI.Card.Css exposing
    ( Class(..)
    , css
    )

import Css exposing (..)
import Css.Namespace
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Importance(..), Size(..))
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)
import Engage.UI.Button.Css as ButtonCss


type Class
    = Card
    | CardHeader
    | CardBody
    | CardAction
    | CardEditButton
    | CardTitle
    | CardSubtitle


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class Card
        [ cardMixin theme ]
    , class CardHeader
        [ headerMixin theme ]
    , class CardBody
        [ bodyMixin theme ]
    , class CardAction
        []
    , class CardEditButton
        [ editMixin theme ]
    , class CardTitle
        []
    , class CardSubtitle
        []
    ]


cardMixin : Theme -> Mixin
cardMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        , border3 (px 1) solid (rgba 0 0 0 0.15)
        , displayFlex
        , flexDirection column
        , flexWrap wrap
        , justifyContent flexStart
        , property "align-content" "flex-start"
        , position relative
        ]


headerMixin : Theme -> Mixin
headerMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        , padding (px 15)
        , fontWeight bold
        , displayFlex
        , flexDirection column
        ]


bodyMixin : Theme -> Mixin
bodyMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        , width (pct 100)
        , padding2 (em 1.5) (em 1.5)
        ]


editMixin : Theme -> Mixin
editMixin theme =
    mixin
        [ descendants
            [ class (ButtonCss.Button Primary Small)
                [ margin zero ]
            ]
        , position absolute
        , top zero
        , right zero
        ]
