module Engage.UI.Dropdown.Css exposing
    ( Class(..)
    , css
    , snippets
    )

import Css exposing (..)
import Css.Namespace
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Class(..), Size(..))
import Engage.Theme as Theme exposing (Theme)
import Engage.ThemeHelper as Theme


type Class
    = Dropdown Size
    | Label


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class (FormControl Large)
        [ children
            [ class Label
                [ labelMixin ]
            , class (Dropdown Large)
                [ dropdownMixin theme
                , largeMixin theme
                ]
            ]
        ]
    , class (FormControl Small)
        [ children
            [ class Label
                [ labelMixin ]
            , class (Dropdown Small)
                [ dropdownMixin theme
                , smallMixin theme
                ]
            ]
        ]
    ]


labelMixin : Mixin
labelMixin =
    mixin
        [ paddingRight (Css.rem 0.5)
        , display block
        ]


dropdownMixin : Theme -> Css.Mixin
dropdownMixin theme =
    let
        themePalette =
            Theme.palette theme
    in
    mixin
        [ border3 (px 1) solid (rgba 221 221 221 1)
        , Theme.backgroundColor themePalette.dropdown.base
        , boxShadow5 inset (px 0) (px 1) (px 3) (rgba 0 0 0 0.15)
        , borderRadius (px 2)
        , width (pct 100)
        , boxSizing borderBox
        , flexGrow (int 1)
        , fontWeight normal
        , padding (em 0.75)
        , property "-moz-appearance" "textfield"
        , pseudoElement "-webkit-outer-spin-button" [ property "-webkit-appearance" "none" ]
        , pseudoElement "-webkit-inner-spin-button" [ property "-webkit-appearance" "none" ]
        ]


largeMixin : Theme -> Css.Mixin
largeMixin theme =
    let
        padding =
            Theme.padding theme (.dropdown >> .padding)

        fontSize =
            Theme.fontSize theme (.dropdown >> .fontSize)
    in
    mixin
        [ padding.base
        , fontSize.base
        ]


smallMixin : Theme -> Css.Mixin
smallMixin theme =
    let
        padding =
            Theme.padding theme (.dropdown >> .padding)

        fontSize =
            Theme.fontSize theme (.dropdown >> .fontSize)
    in
    mixin
        [ padding.small
        , fontSize.small
        ]
