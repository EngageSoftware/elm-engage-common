module Engage.UI.Tooltip.Css exposing (Class(..), css)

import Css exposing (..)
import Css.Namespace
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (MessageType(..), Visibility(..))
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)
import Engage.ThemeHelper as Theme
import Engage.UI.List.Css exposing (Class(..))


type Class
    = Tooltip MessageType Visibility


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    let
        themePalette =
            Theme.palette theme
    in
    [ class (Tooltip Confirmation Visible)
        [ tooltipMixin theme
        , messageTypeMixin .confirmation theme
        , visibleMixin theme
        ]
    , class (Tooltip Error Visible)
        [ tooltipMixin theme
        , messageTypeMixin .error theme
        , visibleMixin theme
        ]
    , class (Tooltip Warning Visible)
        [ tooltipMixin theme
        , messageTypeMixin .warning theme
        , visibleMixin theme
        ]
    , class (Tooltip Info Visible)
        [ tooltipMixin theme
        , messageTypeMixin .info theme
        , visibleMixin theme
        ]
    , class (Tooltip Info Hidden)
        [ tooltipMixin theme
        , messageTypeMixin .info theme
        , hiddenMixin theme
        ]
    , class (Tooltip Error Hidden)
        [ tooltipMixin theme
        , messageTypeMixin .error theme
        , hiddenMixin theme
        ]
    , class (Tooltip Warning Hidden)
        [ tooltipMixin theme
        , messageTypeMixin .warning theme
        , hiddenMixin theme
        ]
    , class (Tooltip Confirmation Hidden)
        [ tooltipMixin theme
        , messageTypeMixin .confirmation theme
        , hiddenMixin theme
        ]
    ]


hiddenMixin : Theme -> Mixin
hiddenMixin theme =
    mixin
        [ tooltipMixin theme
        , opacity (int 0)
        , zIndex (int -1)
        ]


visibleMixin : Theme -> Mixin
visibleMixin theme =
    mixin
        [ tooltipMixin theme
        , display block
        , opacity (int 1)
        ]


tooltipMixin : Theme -> Mixin
tooltipMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        , boxShadow4 (px 1) (px 1) (px 5) (rgba 0 0 0 0.15)
        , maxWidth (Css.em 15)
        , width auto
        , fontSize (Css.em 0.778)
        , padding2 (Css.em 0.5) (Css.em 1)
        , property "transition-property" "opacity"
        , property "transition-duration" "0.35s"
        , property "transition-timing-function" "ease"
        , property "transition-delay" "0s"
        , descendants
            [ class List
                [ padding4 (px 0) (px 0) (px 0) (px 0)
                , margin (px 0)
                , descendants
                    [ class ListItem
                        [ listStyle none ]
                    ]
                ]
            ]
        ]


messageTypeMixin : (Theme.Palette -> Theme.ColorPalette) -> Theme -> Mixin
messageTypeMixin getMessageType theme =
    let
        themePalette =
            Theme.palette theme
    in
    mixin
        [ Theme.color (getMessageType themePalette).base
        , Theme.backgroundColor (getMessageType themePalette).contrast
        ]
