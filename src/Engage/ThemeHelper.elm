module Engage.ThemeHelper exposing
    ( backgroundColor
    , border3
    , borderBottom3
    , borderLeft3
    , borderRight3
    , borderTop3
    , color
    , fill
    , fontFamily
    , fontSize
    , margin
    , messagePalette
    , padding
    , palette
    , spacing
    )

import Css
import Engage.Styles.Class exposing (MessageType(..))
import Engage.Theme as Theme exposing (ColorPalette, Theme)
import Engage.Theme.Dark as Dark
import Engage.Theme.ISMA as ISMA
import Engage.Theme.Light as Light
import Engage.Theme.None as None
import Engage.Unit.Color as Color
import Engage.Unit.FontFamily as FontFamily exposing (..)
import Engage.Unit.Margin as Margin exposing (..)
import Engage.Unit.Padding as Padding exposing (..)
import Engage.Unit.Relative as Relative exposing (..)
import Engage.Unit.Size as Size exposing (Size)


palette : Theme -> Theme.Palette
palette theme =
    case theme of
        Theme.None ->
            None.palette

        Theme.Dark ->
            Dark.palette

        Theme.Light ->
            Light.palette

        Theme.ISMA ->
            ISMA.palette


spacing : Theme -> Theme.Spacing
spacing theme =
    case theme of
        Theme.None ->
            None.spacing

        Theme.Dark ->
            Dark.spacing

        Theme.Light ->
            Light.spacing

        Theme.ISMA ->
            ISMA.spacing


style : Theme -> Theme.Style
style theme =
    case theme of
        Theme.None ->
            None.style

        Theme.Dark ->
            Dark.style

        Theme.Light ->
            Light.style

        Theme.ISMA ->
            ISMA.style


messagePalette : MessageType -> Theme -> ColorPalette
messagePalette messageType theme =
    let
        themePalette =
            palette theme
    in
    case messageType of
        Confirmation ->
            themePalette.confirmation

        Error ->
            themePalette.error

        Warning ->
            themePalette.warning

        Info ->
            themePalette.info


padding : Theme -> (Theme.Spacing -> { base : Padding, small : Relative }) -> { base : Css.Mixin, small : Css.Mixin }
padding theme section =
    let
        themeSpacing =
            spacing theme

        basePadding =
            (section themeSpacing).base
    in
    { base = basePadding |> Padding.toCss
    , small = basePadding |> Relative.applyToPadding (section themeSpacing).small |> Padding.toCss
    }


fontSize : Theme -> (Theme.Style -> { base : Size, small : Relative }) -> { base : Css.Mixin, small : Css.Mixin, baseSize : String, smallSize : String }
fontSize theme section =
    let
        themeStyle =
            style theme

        baseFontSize =
            (section themeStyle).base
    in
    { base = baseFontSize |> Size.fontSize
    , small = baseFontSize |> Relative.applyToSize (section themeStyle).small |> Size.fontSize
    , baseSize = baseFontSize |> Size.toCss
    , smallSize = baseFontSize |> Relative.applyToSize (section themeStyle).small |> Size.toCss
    }


fontFamily : Theme -> (Theme.Style -> FontFamily) -> Css.Mixin
fontFamily theme section =
    let
        toCss fontFamily =
            case fontFamily of
                FontFamily.NotSet ->
                    Css.mixin []

                FontFamily fonts ->
                    Css.fontFamilies fonts
    in
    theme
        |> style
        |> section
        |> toCss


margin : Theme -> (Theme.Spacing -> { base : Margin, small : Relative }) -> { base : Css.Mixin, small : Css.Mixin }
margin theme section =
    let
        themeSpacing =
            spacing theme

        baseMargin =
            (section themeSpacing).base

        margin =
            Css.property "margin"
    in
    { base = baseMargin |> Margin.toCss
    , small = baseMargin |> Relative.applyToMargin (section themeSpacing).small |> Margin.toCss
    }


themeColor : (Css.Color -> Css.Mixin) -> Color.Color -> Css.Mixin
themeColor f color =
    case color of
        Color.ColorNotSet ->
            Css.mixin []

        Color.Color cssColor ->
            f cssColor


backgroundColor : Color.Color -> Css.Mixin
backgroundColor =
    themeColor Css.backgroundColor


color : Color.Color -> Css.Mixin
color =
    themeColor Css.color


fill : Color.Color -> Css.Mixin
fill =
    themeColor Css.fill


themeBorder : (a -> b -> Css.ColorValue { alpha : Float, blue : Int, green : Int, red : Int } -> Css.Mixin) -> a -> b -> Color.Color -> Css.Mixin
themeBorder f width style color =
    case color of
        Color.ColorNotSet ->
            Css.mixin []

        Color.Color cssColor ->
            f width style cssColor



-- NOTE: Css.BorderStyle is not exposed, so these functions cannot be annotated
--border3 : Css.Length compatibleA unitsA -> Css.BorderStyle compatibleB -> Color.Color -> Css.Mixin


border3 =
    themeBorder Css.border3



--borderBottom3 : Css.Length compatibleA unitsA -> Css.BorderStyle compatibleB -> Color.Color -> Css.Mixin


borderBottom3 =
    themeBorder Css.borderBottom3



--borderRight3 : Css.Length compatibleA unitsA -> Css.BorderStyle compatibleB -> Color.Color -> Css.Mixin


borderRight3 =
    themeBorder Css.borderRight3



--borderTop3 : Css.Length compatibleA unitsA -> Css.BorderStyle compatibleB -> Color.Color -> Css.Mixin


borderTop3 =
    themeBorder Css.borderTop3



--borderLeft3 : Css.Length compatibleA unitsA -> Css.BorderStyle compatibleB -> Color.Color -> Css.Mixin


borderLeft3 =
    themeBorder Css.borderLeft3
