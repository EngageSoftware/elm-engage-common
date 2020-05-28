module Engage.Theme.Light exposing
    ( palette
    , spacing
    , style
    )

import Css exposing (hex, rgba)
import Engage.Theme exposing (..)
import Engage.Theme.Base as Base
import Engage.Unit.Color exposing (color)


palette : Palette
palette =
    { error = { base = color <| hex "#DC3A21", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , warning = { base = color <| hex "#FE9839", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , confirmation = { base = color <| hex "#3CB180", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , info = { base = color <| hex "#6555BB", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , buttonPrimary = { base = color <| hex "#162748", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#345CAA" }
    , buttonPrimaryHover = { base = color <| hex "#162748", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#345CAA" }
    , buttonStandard = { base = color <| hex "#F5F5F5", contrast = color <| hex "#333333", tertiary = color <| hex "#FFFFFF" }
    , buttonStandardHover = { base = color <| hex "#F5F5F5", contrast = color <| hex "#333333", tertiary = color <| hex "#FFFFFF" }
    , buttonDivert = { base = color <| hex "#4077ff", contrast = color <| rgba 0 0 0 0, tertiary = color <| hex "#2c53b2" }
    , buttonDivertHover = { base = color <| hex "#4077ff", contrast = color <| rgba 0 0 0 0, tertiary = color <| hex "#2c53b2" }
    , input = { base = color <| rgba 0 0 0 0.05, contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , dropdown = { base = color <| hex "#afafaf", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , wizardHeader = { base = color <| hex "#afafaf", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    }


spacing : Spacing
spacing =
    Base.spacing


style : Style
style =
    Base.style
