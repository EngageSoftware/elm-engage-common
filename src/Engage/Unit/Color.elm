module Engage.Unit.Color exposing
    ( Color(..)
    , color
    , notSet
    )

import Css


type Color
    = ColorNotSet
    | Color Css.Color


notSet : Color
notSet =
    ColorNotSet


color : Css.Color -> Color
color cssColor =
    Color cssColor
