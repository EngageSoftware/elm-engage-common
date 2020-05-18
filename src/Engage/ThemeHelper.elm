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

{-| ThemeHelper

@docs backgroundColor

@docs border3, borderBottom3, borderLeft3, borderRight3, borderTop3
    
@docs color, fill, fontFamily, fontSize

@docs margin, messagePalette, padding, palette, spacing

-}

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


{-| Get the Palette from the theme
-}
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


{-| Get the Spacing from the theme
-}
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


{-| Get the Style from the theme
-}
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


{-| Get the ColorPalette from the theme
-}
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


{-| Get the padding from the theme
-}
padding : Theme -> (Theme.Spacing -> { base : Padding, small : Relative }) -> { base : Css.Style, small : Css.Style }
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


{-| Get the font size from the theme
-}
fontSize : Theme -> (Theme.Style -> { base : Size, small : Relative }) -> { base : Css.Style, small : Css.Style, baseSize : String, smallSize : String }
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


{-| Get the font family from the theme
-}
fontFamily : Theme -> (Theme.Style -> FontFamily) -> Css.Style
fontFamily theme section =
    let
        toCss fontFamily =
            case fontFamily of
                FontFamily.NotSet ->
                    Css.batch []

                FontFamily fonts ->
                    Css.fontFamilies fonts
    in
    theme
        |> style
        |> section
        |> toCss


{-| Get the margin from the theme
-}
margin : Theme -> (Theme.Spacing -> { base : Margin, small : Relative }) -> { base : Css.Style, small : Css.Style }
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


{-| Get the theme color from the theme
-}
themeColor : (Css.Color -> Css.Style) -> Color.Color -> Css.Style
themeColor f color =
    case color of
        Color.ColorNotSet ->
            Css.batch []

        Color.Color cssColor ->
            f cssColor


{-| Get the background color from the theme
-}
backgroundColor : Color.Color -> Css.Style
backgroundColor =
    themeColor Css.backgroundColor


{-| Get the color from the theme
-}
color : Color.Color -> Css.Style
color =
    themeColor Css.color


{-| Get the fill from the theme
-}
fill : Color.Color -> Css.Style
fill =
    themeColor Css.fill


{-| Get the theme border from the theme
-}
themeBorder : (a -> b -> Css.ColorValue { alpha : Float, blue : Int, green : Int, red : Int } -> Css.Style) -> a -> b -> Color.Color -> Css.Style
themeBorder f width style color =
    case color of
        Color.ColorNotSet ->
            Css.batch []

        Color.Color cssColor ->
            f width style cssColor



{-| Get a Css border 3
-}
border3 : Css.Length compatibleA unitsA -> Css.BorderStyle compatibleB -> Color.Color -> Css.Style
border3 =
    themeBorder Css.border3



{-| Get a Css border bottom 3
-}
borderBottom3 : Css.Length compatibleA unitsA -> Css.BorderStyle compatibleB -> Color.Color -> Css.Style
borderBottom3 =
    themeBorder Css.borderBottom3



{-| Get a Css border right 3
-}
borderRight3 : Css.Length compatibleA unitsA -> Css.BorderStyle compatibleB -> Color.Color -> Css.Style
borderRight3 =
    themeBorder Css.borderRight3



{-| Get a Css border top 3
-}
borderTop3 : Css.Length compatibleA unitsA -> Css.BorderStyle compatibleB -> Color.Color -> Css.Style
borderTop3 =
    themeBorder Css.borderTop3


{-| Get a Css border left 3
-}
borderLeft3 : Css.Length compatibleA unitsA -> Css.BorderStyle compatibleB -> Color.Color -> Css.Style
borderLeft3 =
    themeBorder Css.borderLeft3
