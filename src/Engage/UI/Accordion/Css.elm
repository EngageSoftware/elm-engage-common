module Engage.UI.Accordion.Css exposing
    ( AccordionState(..)
    , Class(..)
    , css
    )

import Css exposing (..)
import Css.Foreign exposing (Snippet, class)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)


type Class
    = AccordionList
    | Accordion
    | AccordionHeader
    | AccordionBody AccordionState


type AccordionState
    = AccordionCollapsed
    | AccordionExpanded


css : Namespace -> Theme -> DEPRECATED.Css.File.Stylesheet
css namespace theme =
    (DEPRECATED.Css.File.stylesheet << DEPRECATED.Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class AccordionList
        []
    , class Accordion
        []
    , class AccordionHeader
        [ accordionHeaderMixin theme ]
    , class (AccordionBody AccordionCollapsed)
        [ accordionBodyMixin theme AccordionCollapsed ]
    , class (AccordionBody AccordionExpanded)
        [ accordionBodyMixin theme AccordionExpanded ]
    ]


accordionHeaderMixin : Theme -> Style
accordionHeaderMixin theme =
    batch
        [ BaseCss.normalizeMixin
        , displayFlex
        , alignItems center
        ]


accordionBodyMixin : Theme -> AccordionState -> Style
accordionBodyMixin theme accordionState =
    let
        sharedMixin =
            batch [ BaseCss.normalizeMixin, overflow hidden, margin zero ]
    in
    case accordionState of
        AccordionCollapsed ->
            batch [ sharedMixin, Css.height (px 0) ]

        AccordionExpanded ->
            batch [ sharedMixin, Css.height (pct 100) ]
