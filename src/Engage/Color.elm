module Engage.Color exposing
    ( darken
    , lighten
    )

{-| Color helpers

@docs darken

@docs lighten

-}

import Color
import Color.Manipulate
import Css

{-| Lighten a color by an amount
-}
lighten : Float -> Css.Color -> Css.Color
lighten offset =
    manipulate (Color.Manipulate.lighten offset)


{-| Darken a color by an amount
-}
darken : Float -> Css.Color -> Css.Color
darken offset =
    manipulate (Color.Manipulate.darken offset)


manipulate : (Color.Color -> Color.Color) -> Css.Color -> Css.Color
manipulate manipulator =
    toColor >> manipulator >> toCssColor


toColor : Css.Color -> Color.Color
toColor color =
    Color.rgba color.red color.green color.blue color.alpha


toCssColor : Color.Color -> Css.Color
toCssColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
    Css.rgba (clamp 0 255 red) (clamp 0 255 green) (clamp 0 255 blue) alpha
