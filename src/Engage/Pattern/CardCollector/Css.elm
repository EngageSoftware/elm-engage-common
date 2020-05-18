module Engage.Pattern.CardCollector.Css exposing (Class(..), css)

import Css exposing (..)
import Css.Foreign exposing (Snippet, class, descendants)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)
import Engage.UI.Card.Css as CardCss


type Class
    = CardCollector
    | CardCollectorTitle
    | CardCollectorCards
    | CardCollectorAction


css : Namespace -> Theme -> DEPRECATED.Css.File.Stylesheet
css namespace theme =
    (DEPRECATED.Css.File.stylesheet << DEPRECATED.Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class CardCollector
        [ cardCollectorMixin theme ]
    , class CardCollectorCards
        [ cardsMixin theme ]
    ]


cardCollectorMixin : Theme -> Style
cardCollectorMixin theme =
    batch
        [ BaseCss.normalizeMixin
        ]


cardsMixin : Theme -> Style
cardsMixin theme =
    batch
        [ displayFlex
        , flexWrap wrap
        , descendants
            [ class CardCss.Card
                [ marginRight (em 1), marginTop (em 1) ]
            ]
        , property "grid-template-columns" "repeat(auto-fit, minmax(250px,1fr))"
        , property "display" "grid"
        , property "grid-gap" "1em"
        , marginBottom (em 1)
        ]
