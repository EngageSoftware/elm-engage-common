module Engage.UI.Input.Css exposing
    ( Class(..)
    , css
    , formControlMixin
    , inputMixin
    , labelMixin
    , largeMixin
    , smallMixin
    , snippets
    )

import Css exposing (..)
import Css.Namespace
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Class(..), Importance(..), MessageType(..), Size(..))
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)
import Engage.ThemeHelper as Theme
import Engage.UI.Button.Css exposing (Class(..))
import Engage.UI.Loading.Css as Loading
import Engage.UI.Message.Css exposing (iconHeightPx, iconWidthPx)
import IntlPhoneInput.Css as IntlPhoneInput


type Class
    = Input Size
    | CheckBoxList Size
    | CheckBoxContainer
    | CheckBox Size
    | CheckBoxInput
    | TextArea Size
    | RadioList Size
    | RadioContainer
    | RadioText
    | RadioInput
    | File
    | FileName
    | FileButton Size
    | Label
    | LabelWrapped
    | Date
    | Required


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    formControlStyle theme
        ++ checkBoxStyle theme
        ++ fileStyle theme
        ++ intlPhoneInputStyle
        ++ radioListStyle theme
        ++ requiredIndicatorStyle theme


allSizes : List Size
allSizes =
    [ Small, Large ]


formControlStyle : Theme -> List Snippet
formControlStyle theme =
    allSizes
        |> List.map (formControlWithSize theme)


formControlWithSize : Theme -> Size -> Snippet
formControlWithSize theme size =
    class (FormControl size)
        [ formControlMixin theme size
        , descendants
            [ class Label
                [ labelMixin theme size ]
            , class (Input size)
                [ inputMixin theme
                , case size of
                    Small ->
                        smallMixin theme

                    Large ->
                        largeMixin theme
                ]
            , class (TextArea size)
                [ inputMixin theme
                , case size of
                    Small ->
                        smallMixin theme

                    Large ->
                        largeMixin theme
                , case size of
                    Small ->
                        height (Css.em 5)

                    Large ->
                        height (Css.em 15)
                ]
            ]
        ]


formControlMixin : Theme -> Size -> Mixin
formControlMixin theme size =
    let
        labelFontSize =
            Theme.fontSize theme (.input >> .labelFontSize)

        margin =
            Theme.margin theme (.input >> .margin)
    in
    mixin
        [ BaseCss.normalizeMixin
        , position relative
        , displayFlex
        , flexDirection column
        , flexWrap wrap
        , width (pct 100)
        , case size of
            Small ->
                margin.small

            Large ->
                margin.base
        ]


labelMixin : Theme -> Size -> Mixin
labelMixin theme size =
    let
        labelFontSize =
            Theme.fontSize theme (.input >> .labelFontSize)

        labelPadding =
            Theme.padding theme (.input >> .labelPadding)

        labelFontFamily =
            Theme.fontFamily theme (.input >> .labelFontFamily)
    in
    mixin
        [ case size of
            Large ->
                labelFontSize.base

            Small ->
                labelFontSize.small
        , marginRight (px iconWidthPx)
        , minHeight (px iconHeightPx)
        , lineHeight (px iconHeightPx)
        , case size of
            Large ->
                labelPadding.base

            Small ->
                labelPadding.small
        , labelFontFamily
        , fontWeight bold
        ]


checkBoxStyle : Theme -> List Snippet
checkBoxStyle theme =
    class CheckBoxContainer
        [ BaseCss.normalizeMixin, display block ]
        :: (allSizes
                |> List.map (checkBoxWithSize theme)
           )


checkBoxWithSize : Theme -> Size -> Snippet
checkBoxWithSize theme size =
    class (CheckBox size)
        [ checkBoxMixin
        ]


checkBoxMixin : Mixin
checkBoxMixin =
    mixin
        [ BaseCss.normalizeMixin
        , fontWeight normal
        , displayFlex
        , children
            [ class Label
                [ display inline
                , marginLeft (Css.em 1)
                ]
            ]
        ]


inputMixin : Theme -> Mixin
inputMixin theme =
    let
        themePalette =
            Theme.palette theme
    in
    mixin
        [ border3 (px 1) solid (rgba 221 221 221 1)
        , Theme.backgroundColor themePalette.input.base
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


smallMixin : Theme -> Mixin
smallMixin theme =
    let
        padding =
            Theme.padding theme (.input >> .padding)

        fontSize =
            Theme.fontSize theme (.input >> .fontSize)
    in
    mixin
        [ padding.small
        , fontSize.small
        ]


largeMixin : Theme -> Mixin
largeMixin theme =
    let
        padding =
            Theme.padding theme (.input >> .padding)

        fontSize =
            Theme.fontSize theme (.input >> .fontSize)
    in
    mixin
        [ padding.base
        , fontSize.base
        ]


fileStyle : Theme -> List Snippet
fileStyle theme =
    allSizes |> List.concatMap (fileSnippet theme)


fileSnippet : Theme -> Size -> List Snippet
fileSnippet theme size =
    [ class File
        [ descendants
            [ class (FormControl size)
                [ marginBottom (px 0) ]
            , class Loading.Progress
                [ marginTop (em 0.5) ]
            ]
        ]
    , class (FormControl size)
        [ descendants
            [ class (FileButton size)
                [ property "display" "grid"
                , property "grid-template-columns" "1fr auto"
                , property "grid-gap" "0.5em"
                , alignItems center
                , descendants
                    [ selector "input[type=\"file\"]"
                        [ display none ]
                    , class (Button Primary size)
                        [ display inlineBlock
                        , padding2 (em 0.5) (em 1)
                        , margin (px 0)
                        ]
                    , class FileName
                        [ property "grid-column" "1"
                        , width auto
                        , minHeight (em 3)
                        , overflow hidden
                        , whiteSpace noWrap
                        ]
                    , progressBarSnippet Info theme
                    , progressBarSnippet Error theme
                    , progressBarSnippet Warning theme
                    , progressBarSnippet Confirmation theme
                    ]
                ]
            , class LabelWrapped
                [ width (pct 100) ]
            ]
        ]
    ]


progressBarSnippet : MessageType -> Theme -> Snippet
progressBarSnippet type_ theme =
    class (Loading.Progress type_)
        [ property "grid-column" "1/3" ]


intlPhoneInputStyle : List Snippet
intlPhoneInputStyle =
    [ class IntlPhoneInput.IntlPhoneInput
        [ width (pct 100)
        , descendants
            [ class IntlPhoneInput.CountryPicker
                [ backgroundColor (rgba 0 0 0 0.05)
                , padding2 zero (em 1)
                , hover
                    [ backgroundColor (rgba 0 0 0 0.15)
                    ]
                ]
            ]
        ]
    ]


radioListStyle : Theme -> List Snippet
radioListStyle theme =
    allSizes
        |> List.concatMap (radioListSnippet theme)


radioListSnippet : Theme -> Size -> List Snippet
radioListSnippet theme size =
    let
        fontSize =
            Theme.fontSize theme (.input >> .labelFontSize)
    in
    [ class (RadioList size)
        [ width (pct 100) ]
    , class RadioContainer
        [ displayFlex
        , alignItems center
        , descendants
            [ class RadioInput [ margin zero ]
            ]
        ]
    , class RadioText
        [ fontWeight normal
        , marginLeft (em 0.5)
        , case size of
            Large ->
                fontSize.base

            Small ->
                fontSize.small
        ]
    ]


requiredIndicatorStyle : Theme -> List Snippet
requiredIndicatorStyle theme =
    let
        themePalette =
            Theme.palette theme
    in
    [ class Label
        [ descendants
            [ class Required [ Theme.color themePalette.error.base ]
            ]
        ]
    ]
