module Engage.Theme.ISMA exposing
    ( palette
    , spacing
    , style
    )

import Css exposing (hex, rgba)
import Engage.Theme exposing (..)
import Engage.Theme.Base as Base
import Engage.Unit.Color exposing (color)
import Engage.Unit.FontFamily exposing (..)
import Engage.Unit.Size exposing (px)


palette : Palette
palette =
    { error = { base = color <| hex "#DC3A21", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , warning = { base = color <| hex "#FE9839", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , confirmation = { base = color <| hex "#3CB180", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , info = { base = color <| hex "#284783", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#5d83cd" }
    , buttonPrimary = { base = color <| hex "#4077ff", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#335FCC" }
    , buttonPrimaryHover = { base = color <| hex "#2C53B2", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#264799" }
    , buttonStandard = { base = color <| hex "#DDD", contrast = color <| hex "#333333", tertiary = color <| hex "#D0D0D0" }
    , buttonStandardHover = { base = color <| hex "#DDD", contrast = color <| hex "#333333", tertiary = color <| hex "#D0D0D0" }
    , buttonDivert = { base = color <| hex "#4077ff", contrast = color <| rgba 0 0 0 0, tertiary = color <| hex "#2c53b2" }
    , buttonDivertHover = { base = color <| hex "#4077ff", contrast = color <| rgba 0 0 0 0, tertiary = color <| hex "#2c53b2" }
    , input = { base = color <| rgba 0 0 0 0.05, contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , dropdown = { base = color <| hex "#afafaf", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , wizardHeader = { base = color <| hex "#2B333B", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#DDD" }
    }


spacing : Spacing
spacing =
    Base.spacing


style : Style
style =
    baseStyle


baseStyle : Style
baseStyle =
    Base.customStyle (px 14) (fontFamily [ "sans-serif" ])
