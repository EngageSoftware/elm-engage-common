module Engage.UI.Wizard.Css exposing
    ( Class(..)
    , NavigationStatus(..)
    , SelectedStatus(..)
    , css
    , snippets
    )

import Css exposing (..)
import Css.Elements
import Css.Namespace
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Css as BaseCss
import Engage.Styles.MediaQuery exposing (BreakPoint(..), atMedia)
import Engage.Theme as Theme exposing (Theme)
import Engage.ThemeHelper as Theme
import Engage.UI.Message.Css as MessageCss exposing (Class(..))


type Class
    = Wizard
    | PageIndicator
    | WizardHeader
    | WizardHeaderTitle
    | NavigationArrow NavigationStatus
    | Navigation
    | NavigationList NavigationStatus
    | NavigationItem SelectedStatus
    | WizardBody
    | NavigationControl
    | ShoppingCart


type NavigationStatus
    = Expanded
    | Collapsed


type SelectedStatus
    = Selected
    | NotSelected


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    let
        padding =
            Theme.padding theme (.wizard >> .padding) |> .base

        fontSize =
            Theme.fontSize theme (.wizard >> .fontSize) |> .base

        margin =
            Theme.margin theme (.wizard >> .margin) |> .base
    in
    [ class Wizard
        [ BaseCss.normalizeMixin
        , margin
        , padding
        , fontSize
        , displayFlex
        , flexDirection column
        , descendants
            [ class WizardHeader
                [ wizardHeaderMixin theme
                ]
            , class Navigation
                [ navigationMixin theme
                ]
            , class WizardBody
                [ displayFlex
                , flexDirection column
                , padding3 zero (em 1) (em 1)
                ]
            ]
        ]
    , class NavigationControl
        [ BaseCss.normalizeMixin
        , displayFlex
        , flexDirection column
        , justifyContent flexEnd
        , padding2 (em 1.5) zero
        , borderTop3 (px 1) solid (rgba 0 0 0 0.1)
        ]
    , atMedia Small
        [ class NavigationControl [ flexDirection row ] ]
    ]


wizardHeaderMixin : Theme -> Mixin
wizardHeaderMixin theme =
    let
        palette =
            Theme.palette theme |> .wizardHeader

        themePadding =
            Theme.padding theme (.wizardHeader >> .padding) |> .base

        themeFontSize =
            Theme.fontSize theme (.wizardHeader >> .fontSize) |> .base

        themeMargin =
            Theme.margin theme (.wizardHeader >> .margin) |> .base
    in
    mixin
        [ Theme.backgroundColor palette.base
        , borderRadius4 (em 0.25) (em 0.25) zero zero
        , Theme.borderBottom3 (em 0.25) solid palette.tertiary
        , Theme.color palette.contrast
        , themePadding
        , themeMargin
        , themeFontSize
        , displayFlex
        , flexDirection row
        , alignItems center
        , justifyContent flexEnd
        , position relative
        , descendants
            [ class WizardHeaderTitle
                [ margin2 zero (em 0.5)
                , flexGrow (int 1)
                , Theme.color palette.contrast
                ]
            , class PageIndicator
                [ display inline
                ]
            ]
        ]


navigationMixin : Theme -> Mixin
navigationMixin theme =
    mixin
        [ position relative
        , descendants
            [ class (NavigationList Expanded)
                [ navigationListMixin
                , zIndex (int 1)
                , transforms [ scale 1 ]
                , property "transform-origin" "top"
                , property "transition" "transform 0.2s"
                , descendants
                    [ class (NavigationItem Selected)
                        [ opacity (int 1)
                        , property "transition" "opacity 0.2s 0.2s"
                        ]
                    , class (NavigationItem NotSelected)
                        [ opacity (int 1)
                        , property "transition" "opacity 0.2s 0.2s"
                        ]
                    ]
                ]
            , class (NavigationList Collapsed)
                [ navigationListMixin
                , opacity zero
                , transforms [ scale2 1 0 ]
                , descendants
                    [ class (NavigationItem Selected)
                        [ opacity zero
                        ]
                    , class (NavigationItem NotSelected)
                        [ opacity zero
                        ]
                    ]
                ]
            , class (NavigationItem Selected)
                [ navigationItemMixin theme Selected
                ]
            , class (NavigationItem NotSelected)
                [ navigationItemMixin theme NotSelected
                ]
            , class (NavigationArrow Expanded)
                [ navigationArrowMixin theme
                , descendants
                    [ class Chevron
                        [ transform (rotate (deg 180))
                        , property "transition" "transform 0.5s"
                        ]
                    ]
                ]
            , class (NavigationArrow Collapsed)
                [ navigationArrowMixin theme
                , descendants
                    [ class Chevron
                        [ transform (rotate (deg 360))
                        , property "transition" "transform 0.5s"
                        ]
                    ]
                ]
            ]
        ]


navigationArrowMixin : Theme -> Mixin
navigationArrowMixin theme =
    let
        palette =
            Theme.palette theme |> .wizardHeader
    in
    mixin
        [ backgroundColor transparent
        , border2 zero none
        , fontSize (em 1)
        , fontWeight bold
        , lineHeight (int 1)
        , marginLeft (em 0.5)
        , padding2 (em 0.25) (em 0.5)
        , hover
            [ cursor pointer
            , opacity (num 0.5)
            ]
        , descendants
            [ class Chevron
                [ Theme.fill palette.contrast
                ]
            ]
        ]


navigationListMixin : Mixin
navigationListMixin =
    mixin
        [ position absolute
        , top (pct 100)
        , right (Css.rem -0.5)
        , fontSize (em 0.722)
        , minWidth (em 18)
        , lineHeight (num 1.2)
        , margin zero
        , marginTop (Css.rem 0.5)
        , padding (em 1.5)
        , backgroundColor (hex "eee")
        , borderRadius4 zero zero (em 0.25) (em 0.25)
        ]


navigationItemMixin : Theme -> SelectedStatus -> Mixin
navigationItemMixin theme selectedStatus =
    mixin
        [ listStyle none
        , padding (em 1)
        , paddingLeft (em 2.5)
        , borderBottom3 (px 1) solid (rgba 0 0 0 0.05)
        , position relative
        , fontWeight normal
        , firstOfType
            [ borderTop3 (px 1) solid (rgba 0 0 0 0.05)
            ]
        , before
            [ position absolute
            , left zero
            , top zero
            , bottom zero
            , margin auto
            , height (em 1.5)
            , width (em 1.5)
            , fontSize (em 1)
            , lineHeight (int 1)
            , textAlign center
            , padding (em 0.25)
            , color (rgba 0 0 0 0.25)
            , backgroundColor (rgba 0 0 0 0.05)
            , borderRadius (pct 50)
            , opacity (num 0.5)
            , property "content" "attr(data-index)"
            ]
        , descendants
            [ Css.Elements.a
                [ color (hex "#444")
                , textDecoration none
                , hover
                    [ color lighterColorLink ]
                ]
            ]
        , case selectedStatus of
            Selected ->
                mixin
                    [ before
                        [ backgroundColor lighterColorLink
                        , opacity (int 1)
                        , color (hex "#fff")
                        , fontWeight normal
                        ]
                    ]

            NotSelected ->
                mixin
                    [ color (rgba 0 0 0 0.25) ]
        ]


lighterColorLink : Color
lighterColorLink =
    hex "#2b333b"
