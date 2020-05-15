module Engage.Theme.Dark exposing
    ( palette
    , spacing
    , style
    )

import Css exposing (hex, rgba)
import Engage.Theme exposing (..)
import Engage.Theme.Base as Base
import Engage.Unit.Color as Color exposing (color)


palette : Palette
palette =
    { error = { base = color <| hex "#FF3860", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , warning = { base = color <| hex "#FFDD57", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , confirmation = { base = color <| hex "#FFDD57", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , info = { base = color <| hex "#3273DC", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , buttonPrimary = { base = color <| hex "#018470", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , buttonPrimaryHover = { base = color <| hex "#018470", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , buttonStandard = { base = color <| hex "#F5F5F5", contrast = color <| hex "#555555", tertiary = color <| hex "#FFFFFF" }
    , buttonStandardHover = { base = color <| hex "#F5F5F5", contrast = color <| hex "#555555", tertiary = color <| hex "#FFFFFF" }
    , buttonDivert = { base = color <| hex "#4077ff", contrast = color <| rgba 0 0 0 0, tertiary = color <| hex "#2c53b2" }
    , buttonDivertHover = { base = color <| hex "#4077ff", contrast = color <| rgba 0 0 0 0, tertiary = color <| hex "#2c53b2" }
    , input = { base = color <| hex "#018470", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , dropdown = { base = color <| hex "#018470", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , wizardHeader = { base = color <| hex "#018470", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    }


spacing : Spacing
spacing =
    Base.spacing


style : Style
style =
    Base.style
