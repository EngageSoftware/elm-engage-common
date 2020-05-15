module Engage.Theme exposing
    ( ColorPalette
    , Decoration
    , DecorationOnly
    , LabelTypography
    , LabelTypographyOnly
    , Palette
    , Spacing
    , SpacingData
    , Style
    , Theme(..)
    , ThemeData
    , Typography
    , TypographyOnly
    , WithLabel
    , WithLabelOnly
    , fromString
    , toDropdownItem
    , toString
    )

import Dropdown
import Engage.Unit.Border as Border exposing (Border)
import Engage.Unit.Color as Color
import Engage.Unit.FontFamily as FontFamily exposing (FontFamily)
import Engage.Unit.Margin as Margin exposing (Margin)
import Engage.Unit.Padding as Padding exposing (Padding)
import Engage.Unit.Relative as Relative exposing (Relative)
import Engage.Unit.Size as Size exposing (Size)


type Theme
    = Light
    | Dark
    | ISMA
    | None


toString : Theme -> String
toString theme =
    Basics.toString theme


fromString : Theme -> String -> Theme
fromString default str =
    case str of
        "Light" ->
            Light

        "Dark" ->
            Dark

        _ ->
            default


toDropdownItem : Theme -> Dropdown.Item
toDropdownItem theme =
    { value = toString theme, text = toString theme, enabled = True }


type alias Palette =
    { buttonPrimary : ColorPalette
    , buttonPrimaryHover : ColorPalette
    , buttonStandard : ColorPalette
    , buttonStandardHover : ColorPalette
    , buttonDivert : ColorPalette
    , buttonDivertHover : ColorPalette
    , input : ColorPalette
    , dropdown : ColorPalette
    , error : ColorPalette
    , warning : ColorPalette
    , confirmation : ColorPalette
    , info : ColorPalette
    , wizardHeader : ColorPalette
    }


type alias ColorPalette =
    { base : Color.Color, contrast : Color.Color, tertiary : Color.Color }


type alias Spacing =
    ThemeData SpacingDataOnly (SpacingData WithLabelOnly) (SpacingData WithLabelOnly) SpacingDataOnly SpacingDataOnly


type alias Style =
    ThemeData (Typography (LabelTypography DecorationOnly)) (Typography (LabelTypography DecorationOnly)) (Typography (LabelTypography DecorationOnly)) (Typography DecorationOnly) (Typography DecorationOnly)


type alias ThemeData button input dropdown wizard wizardHeader =
    { button : button
    , input : input
    , dropdown : dropdown
    , wizard : wizard
    , wizardHeader : wizardHeader
    }


type alias SpacingDataOnly =
    SpacingData {}


type alias SpacingData modifier =
    { modifier
        | padding :
            { base : Padding
            , small : Relative
            }
        , margin :
            { base : Margin
            , small : Relative
            }
    }


type alias WithLabelOnly =
    WithLabel {}


type alias WithLabel a =
    { a
        | labelMargin :
            { base : Margin
            , small : Relative
            }
        , labelPadding :
            { base : Padding
            , small : Relative
            }
    }


type alias DecorationOnly =
    Decoration {}


type alias Decoration a =
    { a | border : Border }


type alias TypographyOnly =
    Typography {}


type alias Typography a =
    { a
        | fontFamily : FontFamily
        , fontSize :
            { base : Size
            , small : Relative
            }
    }


type alias LabelTypographyOnly =
    LabelTypography {}


type alias LabelTypography a =
    { a
        | labelFontFamily : FontFamily
        , labelFontSize :
            { base : Size
            , small : Relative
            }
    }
