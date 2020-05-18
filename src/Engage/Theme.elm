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

{-| Basic Theme types

# Types
@docs ColorPalette, Decoration, DecorationOnly, LabelTypography, LabelTypographyOnly, Palette, Spacing, SpacingData, Style, Theme, ThemeData, Typography, TypographyOnly, WithLabel, WithLabelOnly

# Helpers
@docs fromString, toDropdownItem, toString

-}

import Dropdown
import Engage.Unit.Border as Border exposing (Border)
import Engage.Unit.Color as Color
import Engage.Unit.FontFamily as FontFamily exposing (FontFamily)
import Engage.Unit.Margin as Margin exposing (Margin)
import Engage.Unit.Padding as Padding exposing (Padding)
import Engage.Unit.Relative as Relative exposing (Relative)
import Engage.Unit.Size as Size exposing (Size)


{-| A theme
-}
type Theme
    = Light
    | Dark
    | ISMA
    | None


{-| Get the string value of a theme
-}
toString : Theme -> String
toString theme =
    Basics.toString theme


{-| Get a theme from a string
-}
fromString : Theme -> String -> Theme
fromString default str =
    case str of
        "Light" ->
            Light

        "Dark" ->
            Dark

        _ ->
            default


{-| Get a dropdown item for the theme
-}
toDropdownItem : Theme -> Dropdown.Item
toDropdownItem theme =
    { value = toString theme, text = toString theme, enabled = True }


{-| A theme palatte
-}
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


{-| A color palette
-}
type alias ColorPalette =
    { base : Color.Color, contrast : Color.Color, tertiary : Color.Color }


{-| A spacing type
-}
type alias Spacing =
    ThemeData SpacingDataOnly (SpacingData WithLabelOnly) (SpacingData WithLabelOnly) SpacingDataOnly SpacingDataOnly

{-| A style type
-}
type alias Style =
    ThemeData (Typography (LabelTypography DecorationOnly)) (Typography (LabelTypography DecorationOnly)) (Typography (LabelTypography DecorationOnly)) (Typography DecorationOnly) (Typography DecorationOnly)


{-| A theme data type
-}
type alias ThemeData button input dropdown wizard wizardHeader =
    { button : button
    , input : input
    , dropdown : dropdown
    , wizard : wizard
    , wizardHeader : wizardHeader
    }


{-| A spacing data only type
-}
type alias SpacingDataOnly =
    SpacingData {}


{-| A spacing data type
-}
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


{-| A with label only type
-}
type alias WithLabelOnly =
    WithLabel {}


{-| A with label type
-}
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


{-| A decoration only type
-}
type alias DecorationOnly =
    Decoration {}


{-| A decoration type
-}
type alias Decoration a =
    { a | border : Border }


{-| A typography only type
-}
type alias TypographyOnly =
    Typography {}


{-| A typography type
-}
type alias Typography a =
    { a
        | fontFamily : FontFamily
        , fontSize :
            { base : Size
            , small : Relative
            }
    }


{-| A label typography only type
-}
type alias LabelTypographyOnly =
    LabelTypography {}


{-| A label typography type
-}
type alias LabelTypography a =
    { a
        | labelFontFamily : FontFamily
        , labelFontSize :
            { base : Size
            , small : Relative
            }
    }
