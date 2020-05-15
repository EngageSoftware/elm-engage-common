module Engage.UI.Message.Css exposing
    ( Class(..)
    , css
    , iconHeightPx
    , iconWidthPx
    , snippets
    )

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (MessageType(..), Visibility(..))
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)
import Engage.ThemeHelper as Theme
import Engage.UI.Tooltip.Css exposing (Class(..))


type Class
    = Message MessageType
    | Icon MessageType
    | IconContainer
    | InlineMessage MessageType
    | ControlMessage MessageType
    | Chevron


iconWidthPx : Float
iconWidthPx =
    24


iconHeightPx : Float
iconHeightPx =
    24


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
    [ class (Icon Error)
        [ iconMixin theme
        , Theme.fill themePalette.error.base
        ]
    , class (Icon Info)
        [ iconMixin theme
        , Theme.fill themePalette.info.base
        ]
    , class (Icon Confirmation)
        [ iconMixin theme
        , Theme.fill themePalette.confirmation.base
        ]
    , class (Icon Warning)
        [ iconMixin theme
        , Theme.fill themePalette.warning.base
        ]
    , class (Message Confirmation)
        [ validationMixin theme
        , Theme.backgroundColor themePalette.confirmation.base
        , Theme.color themePalette.confirmation.contrast
        ]
    , class (Message Error)
        [ validationMixin theme
        , Theme.backgroundColor themePalette.error.base
        , Theme.color themePalette.error.contrast
        ]
    , class (Message Warning)
        [ validationMixin theme
        , Theme.backgroundColor themePalette.warning.base
        , Theme.color themePalette.warning.contrast
        ]
    , class (Message Info)
        [ validationMixin theme
        , Theme.backgroundColor themePalette.info.base
        , Theme.color themePalette.info.contrast
        ]
    , class IconContainer
        [ iconContainerMixin theme ]
    , controlMessageSnippet Confirmation theme
    , controlMessageSnippet Error theme
    , controlMessageSnippet Warning theme
    , controlMessageSnippet Info theme
    ]


iconMixin : Theme -> Mixin
iconMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        ]


controlMessageSnippet : MessageType -> Theme -> Snippet
controlMessageSnippet type_ theme =
    class (ControlMessage type_)
        [ BaseCss.normalizeMixin
        , descendants
            [ class (Icon type_)
                [ controlMessageIconMixin theme
                ]
            , class (Tooltip type_ Visible)
                [ controlMessageTooltipMixin theme ]
            , class (Tooltip type_ Hidden)
                [ controlMessageTooltipMixin theme
                , zIndex (int -1)
                ]
            ]
        ]


controlMessageIconMixin : Theme -> Mixin
controlMessageIconMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        , cursor pointer
        ]


controlMessageTooltipMixin : Theme -> Mixin
controlMessageTooltipMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        , position absolute
        , bottom (pct 100)
        , right (px iconWidthPx)
        , zIndex (int 1)
        ]


validationMixin : Theme -> Mixin
validationMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        , padding (Css.em 1)
        , margin2 (Css.em 0.5) (px 0)
        , descendants
            [ ul
                [ margin4 (px 0) (px 0) (px 0) (Css.em 1.5)
                , padding (px 0)
                ]
            , p [ margin zero ]
            ]
        ]


iconContainerMixin : Theme -> Mixin
iconContainerMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        , backgroundColor transparent
        , borderStyle none
        , padding (px 0)
        , margin (px 0)
        , position absolute
        , top (Css.em 0)
        , right (px 0)
        ]
