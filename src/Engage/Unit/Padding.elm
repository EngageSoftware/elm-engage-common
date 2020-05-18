module Engage.Unit.Padding exposing (Padding(..), fromList, notSet, padding, padding2, padding3, padding4, toCss, toList)

import Css
import Engage.Unit.Size as Size exposing (..)


type Padding
    = Padding Size
    | Padding2 Size Size
    | Padding3 Size Size Size
    | Padding4 Size Size Size Size
    | NotSet


notSet : Padding
notSet =
    NotSet


padding : Size -> Padding
padding =
    Padding


padding2 : Size -> Size -> Padding
padding2 =
    Padding2


padding3 : Size -> Size -> Size -> Padding
padding3 =
    Padding3


padding4 : Size -> Size -> Size -> Size -> Padding
padding4 =
    Padding4


toList : Padding -> List Size
toList unit =
    case unit of
        NotSet ->
            []

        Padding unit ->
            [ unit ]

        Padding2 unit1 unit2 ->
            [ unit1, unit2 ]

        Padding3 unit1 unit2 unit3 ->
            [ unit1, unit2, unit3 ]

        Padding4 unit1 unit2 unit3 unit4 ->
            [ unit1, unit2, unit3, unit4 ]


fromList : List Size -> Padding
fromList units =
    case units of
        [] ->
            NotSet

        unit1 :: unit2 :: unit3 :: unit4 :: _ ->
            padding4 unit1 unit2 unit3 unit4

        unit1 :: unit2 :: unit3 :: [] ->
            padding3 unit1 unit2 unit3

        unit1 :: unit2 :: [] ->
            padding2 unit1 unit2

        unit1 :: [] ->
            padding unit1


toCss : Padding -> Css.Style
toCss padding =
    case padding of
        NotSet ->
            Css.batch []

        Padding size ->
            Css.property "padding" (Size.toCss size)

        Padding2 size1 size2 ->
            Css.property "padding" (Size.toCss size1 ++ " " ++ Size.toCss size2)

        Padding3 size1 size2 size3 ->
            Css.property "padding" (Size.toCss size1 ++ " " ++ Size.toCss size2 ++ " " ++ Size.toCss size3)

        Padding4 size1 size2 size3 size4 ->
            Css.property "padding" (Size.toCss size1 ++ " " ++ Size.toCss size2 ++ " " ++ Size.toCss size3 ++ " " ++ Size.toCss size4)



-- Css.padding4 (Size.toCss unit1) (Size.toCss unit2) (Size.toCss unit3) (Size.toCss unit4)
