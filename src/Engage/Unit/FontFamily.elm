module Engage.Unit.FontFamily exposing (FontFamily(..), fontFamily, notSet)


type FontFamily
    = NotSet
    | FontFamily (List String)


notSet : FontFamily
notSet =
    NotSet


fontFamily : List String -> FontFamily
fontFamily fontFamilies =
    FontFamily fontFamilies
