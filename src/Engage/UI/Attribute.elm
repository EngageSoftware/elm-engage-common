module Engage.UI.Attribute exposing (process)


process : config -> List (config -> config) -> config
process initialAttribute configs =
    List.foldl (\f config -> f config) initialAttribute configs
