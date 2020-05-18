module Engage.UI.Tooltip.Css exposing (Class(..), css)

import Css exposing (..)
import Css.Foreign exposing (Snippet, class, descendants)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class as Class exposing (MessageType(..))
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)
import Engage.ThemeHelper as Theme
import Engage.UI.List.Css exposing (Class(..))


type Class
    = Tooltip MessageType Class.Visibility


css : Namespace -> Theme -> DEPRECATED.Css.File.Stylesheet
css namespace theme =
    (DEPRECATED.Css.File.stylesheet << DEPRECATED.Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    let
        themePalette =
            Theme.palette theme
    in
    [ class (Tooltip Confirmation Class.Visible)
        [ tooltipMixin theme
        , messageTypeMixin .confirmation theme
        , visibleMixin theme
        ]
    , class (Tooltip Error Class.Visible)
        [ tooltipMixin theme
        , messageTypeMixin .error theme
        , visibleMixin theme
        ]
    , class (Tooltip Warning Class.Visible)
        [ tooltipMixin theme
        , messageTypeMixin .warning theme
        , visibleMixin theme
        ]
    , class (Tooltip Info Class.Visible)
        [ tooltipMixin theme
        , messageTypeMixin .info theme
        , visibleMixin theme
        ]
    , class (Tooltip Info Class.Hidden)
        [ tooltipMixin theme
        , messageTypeMixin .info theme
        , hiddenMixin theme
        ]
    , class (Tooltip Error Class.Hidden)
        [ tooltipMixin theme
        , messageTypeMixin .error theme
        , hiddenMixin theme
        ]
    , class (Tooltip Warning Class.Hidden)
        [ tooltipMixin theme
        , messageTypeMixin .warning theme
        , hiddenMixin theme
        ]
    , class (Tooltip Confirmation Class.Hidden)
        [ tooltipMixin theme
        , messageTypeMixin .confirmation theme
        , hiddenMixin theme
        ]
    ]


hiddenMixin : Theme -> Style
hiddenMixin theme =
    batch
        [ tooltipMixin theme
        , opacity (int 0)
        , zIndex (int -1)
        ]


visibleMixin : Theme -> Style
visibleMixin theme =
    batch
        [ tooltipMixin theme
        , display block
        , opacity (int 1)
        ]


tooltipMixin : Theme -> Style
tooltipMixin theme =
    batch
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


messageTypeMixin : (Theme.Palette -> Theme.ColorPalette) -> Theme -> Style
messageTypeMixin getMessageType theme =
    let
        themePalette =
            Theme.palette theme
    in
    batch
        [ Theme.color (getMessageType themePalette).base
        , Theme.backgroundColor (getMessageType themePalette).contrast
        ]
