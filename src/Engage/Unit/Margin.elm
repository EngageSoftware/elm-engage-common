module Engage.Unit.Margin exposing (Margin(..), fromList, margin, margin2, margin3, margin4, notSet, toCss, toList)

import Css
import Engage.Unit.Size as Size exposing (..)


type Margin
    = Margin Size
    | Margin2 Size Size
    | Margin3 Size Size Size
    | Margin4 Size Size Size Size
    | NotSet


notSet : Margin
notSet =
    NotSet


margin : Size -> Margin
margin =
    Margin


margin2 : Size -> Size -> Margin
margin2 =
    Margin2


margin3 : Size -> Size -> Size -> Margin
margin3 =
    Margin3


margin4 : Size -> Size -> Size -> Size -> Margin
margin4 =
    Margin4


toList : Margin -> List Size
toList unit =
    case unit of
        NotSet ->
            []

        Margin unit1 ->
            [ unit1 ]

        Margin2 unit1 unit2 ->
            [ unit1, unit2 ]

        Margin3 unit1 unit2 unit3 ->
            [ unit1, unit2, unit3 ]

        Margin4 unit1 unit2 unit3 unit4 ->
            [ unit1, unit2, unit3, unit4 ]


fromList : List Size -> Margin
fromList units =
    case units of
        [] ->
            margin (em 0)

        unit1 :: unit2 :: unit3 :: unit4 :: _ ->
            margin4 unit1 unit2 unit3 unit4

        unit1 :: unit2 :: unit3 :: [] ->
            margin3 unit1 unit2 unit3

        unit1 :: unit2 :: [] ->
            margin2 unit1 unit2

        unit1 :: [] ->
            margin unit1


toCss : Margin -> Css.Style
toCss marginValue =
    case marginValue of
        NotSet ->
            Css.batch []

        Margin size ->
            Css.property "margin" (Size.toCss size)

        Margin2 size1 size2 ->
            Css.property "margin" (Size.toCss size1 ++ " " ++ Size.toCss size2)

        Margin3 size1 size2 size3 ->
            Css.property "margin" (Size.toCss size1 ++ " " ++ Size.toCss size2 ++ " " ++ Size.toCss size3)

        Margin4 size1 size2 size3 size4 ->
            Css.property "margin" (Size.toCss size1 ++ " " ++ Size.toCss size2 ++ " " ++ Size.toCss size3 ++ " " ++ Size.toCss size4)



-- Css.margin4 (Size.toCss unit1) (Size.toCss unit2) (Size.toCss unit3) (Size.toCss unit4)
