module Engage.UI.Button.Css exposing
    ( Class(..)
    , css
    , snippets
    )

import Css exposing (..)
import Css.Namespace
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Importance(..), Size(..))
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)
import Engage.ThemeHelper as Theme


type Class
    = BaseButton
    | Button Importance Size


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class BaseButton
        [ buttonMixin theme ]
    , class (Button Standard Small)
        [ smallMixin theme
        ]
    , class (Button Primary Small)
        [ smallMixin theme
        , primaryMixin theme
        ]
    , class (Button Standard Small)
        [ smallMixin theme
        , standardMixin theme
        ]
    , class (Button Divert Small)
        [ smallMixin theme
        , divertMixin theme
        ]
    , class (Button Primary Large)
        [ largeMixin theme
        , primaryMixin theme
        ]
    , class (Button Standard Large)
        [ largeMixin theme
        , standardMixin theme
        ]
    , class (Button Divert Large)
        [ largeMixin theme
        , divertMixin theme
        ]
    ]


buttonMixin : Theme -> Mixin
buttonMixin theme =
    case theme of
        Theme.None ->
            mixin []

        _ ->
            mixin
                [ BaseCss.normalizeMixin
                , fontSize (em 0.875)
                , lineHeight (num 1.2)
                , border3 (px 2) solid transparent
                , fontWeight normal
                , cursor pointer
                , borderRadius (px 2)
                , property "transition" "background-color 0.4s ease 0s, border-color 0.4s ease 0s, color 0.4s ease 0s"
                , textShadow4 (px 1) (px 1) (px 2) (rgba 0 0 0 0.25)
                ]



-- SIZES


smallMixin : Theme -> Mixin
smallMixin theme =
    let
        padding =
            Theme.padding theme (.button >> .padding)

        fontSize =
            Theme.fontSize theme (.button >> .fontSize)

        margin =
            Theme.margin theme (.button >> .margin)
    in
    mixin
        [ padding.small
        , fontSize.small
        , margin.small
        ]


largeMixin : Theme -> Mixin
largeMixin theme =
    let
        padding =
            Theme.padding theme (.button >> .padding)

        fontSize =
            Theme.fontSize theme (.button >> .fontSize)

        margin =
            Theme.margin theme (.button >> .margin)
    in
    mixin
        [ padding.base
        , fontSize.base
        , margin.base
        ]



-- IMPORTANCe


primaryMixin : Theme -> Mixin
primaryMixin theme =
    let
        palette =
            Theme.palette theme
    in
    mixin
        [ Theme.backgroundColor palette.buttonPrimary.base
        , Theme.color palette.buttonPrimary.contrast
        , Theme.border3 (px 2) solid palette.buttonPrimary.tertiary
        , hover
            [ Theme.backgroundColor palette.buttonPrimaryHover.base
            , Theme.color palette.buttonPrimaryHover.contrast
            , Theme.border3 (px 2) solid palette.buttonPrimaryHover.tertiary
            ]
        ]


standardMixin : Theme -> Mixin
standardMixin theme =
    let
        palette =
            Theme.palette theme
    in
    mixin
        [ Theme.backgroundColor palette.buttonStandard.base
        , Theme.color palette.buttonStandard.contrast
        , Theme.border3 (px 2) solid palette.buttonStandard.tertiary
        , hover
            [ Theme.backgroundColor palette.buttonStandardHover.base
            , Theme.color palette.buttonStandardHover.contrast
            , Theme.border3 (px 2) solid palette.buttonStandardHover.tertiary
            ]
        ]


divertMixin : Theme -> Mixin
divertMixin theme =
    let
        palette =
            Theme.palette theme
    in
    mixin
        [ Theme.backgroundColor palette.buttonDivert.contrast
        , Theme.color palette.buttonDivert.base
        ]
