module Engage.UI.Button.Css exposing
    ( Class(..)
    , css
    , snippets
    )

{-| UI.Button.Css

@docs Class

@docs css, snippets

-}

import Css exposing (..)
import Css.Foreign exposing (Snippet, class)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Importance(..), Size(..))
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)
import Engage.ThemeHelper as Theme


{-| The Class type
-}
type Class
    = BaseButton
    | Button Importance Size


{-| Get the css
-}
css : Namespace -> Theme -> DEPRECATED.Css.File.Stylesheet
css namespace theme =
    (DEPRECATED.Css.File.stylesheet << DEPRECATED.Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


{-| Get the snippets
-}
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


buttonMixin : Theme -> Style
buttonMixin theme =
    case theme of
        Theme.None ->
            batch []

        _ ->
            batch
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


smallMixin : Theme -> Style
smallMixin theme =
    let
        padding =
            Theme.padding theme (.button >> .padding)

        fontSize =
            Theme.fontSize theme (.button >> .fontSize)

        margin =
            Theme.margin theme (.button >> .margin)
    in
    batch
        [ padding.small
        , fontSize.small
        , margin.small
        ]


largeMixin : Theme -> Style
largeMixin theme =
    let
        padding =
            Theme.padding theme (.button >> .padding)

        fontSize =
            Theme.fontSize theme (.button >> .fontSize)

        margin =
            Theme.margin theme (.button >> .margin)
    in
    batch
        [ padding.base
        , fontSize.base
        , margin.base
        ]



-- IMPORTANCe


primaryMixin : Theme -> Style
primaryMixin theme =
    let
        palette =
            Theme.palette theme
    in
    batch
        [ Theme.backgroundColor palette.buttonPrimary.base
        , Theme.color palette.buttonPrimary.contrast
        , Theme.border3 (px 2) solid palette.buttonPrimary.tertiary
        , hover
            [ Theme.backgroundColor palette.buttonPrimaryHover.base
            , Theme.color palette.buttonPrimaryHover.contrast
            , Theme.border3 (px 2) solid palette.buttonPrimaryHover.tertiary
            ]
        ]


standardMixin : Theme -> Style
standardMixin theme =
    let
        palette =
            Theme.palette theme
    in
    batch
        [ Theme.backgroundColor palette.buttonStandard.base
        , Theme.color palette.buttonStandard.contrast
        , Theme.border3 (px 2) solid palette.buttonStandard.tertiary
        , hover
            [ Theme.backgroundColor palette.buttonStandardHover.base
            , Theme.color palette.buttonStandardHover.contrast
            , Theme.border3 (px 2) solid palette.buttonStandardHover.tertiary
            ]
        ]


divertMixin : Theme -> Style
divertMixin theme =
    let
        palette =
            Theme.palette theme
    in
    batch
        [ Theme.backgroundColor palette.buttonDivert.contrast
        , Theme.color palette.buttonDivert.base
        ]
