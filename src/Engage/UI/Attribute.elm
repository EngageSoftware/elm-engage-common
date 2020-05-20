module Engage.UI.Attribute exposing (process)

{-| UI.Attribute

@docs process

-}


{-| Process an attribute
-}
process : config -> List (config -> config) -> config
process initialAttribute configs =
    List.foldl (\f config -> f config) initialAttribute configs
