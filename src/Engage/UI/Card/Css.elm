module Engage.UI.Card.Css exposing
    ( Class(..)
    , css
    )

{-| UI.Card.Css

@docs Class

@docs css

-}

import Css exposing (..)
import Css.Foreign exposing (Snippet, class, descendants)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Importance(..), Size(..))
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)
import Engage.UI.Button.Css as ButtonCss


{-| The Class type
-}
type Class
    = Card
    | CardHeader
    | CardBody
    | CardAction
    | CardEditButton
    | CardTitle
    | CardSubtitle


{-| Get the css
-}
css : Namespace -> Theme -> DEPRECATED.Css.File.Stylesheet
css namespace theme =
    (DEPRECATED.Css.File.stylesheet << DEPRECATED.Css.Namespace.namespace (Namespace.toString namespace))
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


cardMixin : Theme -> Style
cardMixin theme =
    batch
        [ BaseCss.normalizeMixin
        , border3 (px 1) solid (rgba 0 0 0 0.15)
        , displayFlex
        , flexDirection column
        , flexWrap wrap
        , justifyContent flexStart
        , property "align-content" "flex-start"
        , position relative
        ]


headerMixin : Theme -> Style
headerMixin theme =
    batch
        [ BaseCss.normalizeMixin
        , padding (px 15)
        , fontWeight bold
        , displayFlex
        , flexDirection column
        ]


bodyMixin : Theme -> Style
bodyMixin theme =
    batch
        [ BaseCss.normalizeMixin
        , width (pct 100)
        , padding2 (em 1.5) (em 1.5)
        ]


editMixin : Theme -> Style
editMixin theme =
    batch
        [ descendants
            [ class (ButtonCss.Button Primary Small)
                [ margin zero ]
            ]
        , position absolute
        , top zero
        , right zero
        ]
