module Engage.UI.Accordion.Css exposing
    ( AccordionState(..)
    , Class(..)
    , css
    )

import Css exposing (..)
import Css.Namespace
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


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
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


accordionHeaderMixin : Theme -> Mixin
accordionHeaderMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        , displayFlex
        , alignItems center
        ]


accordionBodyMixin : Theme -> AccordionState -> Mixin
accordionBodyMixin theme accordionState =
    let
        sharedMixin =
            mixin [ BaseCss.normalizeMixin, overflow hidden, margin zero ]
    in
    case accordionState of
        AccordionCollapsed ->
            mixin [ sharedMixin, Css.height (px 0) ]

        AccordionExpanded ->
            mixin [ sharedMixin, Css.height (pct 100) ]
