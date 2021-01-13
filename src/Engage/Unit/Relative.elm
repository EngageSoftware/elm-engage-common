module Engage.Unit.Relative exposing (Relative(..), applyToMargin, applyToPadding, applyToSize, relative, relative4)

import Engage.Unit.Margin as Margin exposing (Margin)
import Engage.Unit.Padding as Padding exposing (Padding)
import Engage.Unit.Size as Size exposing (Size)


type Relative
    = Relative Float
    | Relative2 Float Float
    | Relative3 Float Float Float
    | Relative4 Float Float Float Float


relative : Float -> Relative
relative =
    Relative


relative2 : Float -> Float -> Relative
relative2 =
    Relative2


relative3 : Float -> Float -> Float -> Relative
relative3 =
    Relative3


relative4 : Float -> Float -> Float -> Float -> Relative
relative4 =
    Relative4


singular : Relative -> Float
singular relativeValue =
    case relativeValue of
        Relative relative1 ->
            relative1

        Relative2 relative1 _ ->
            relative1

        Relative3 relative1 _ _ ->
            relative1

        Relative4 relative1 _ _ _ ->
            relative1


toList : Relative -> List Float
toList relativeValue =
    case relativeValue of
        Relative relative1 ->
            [ relative1 ]

        Relative2 relative1 relativeValue2 ->
            [ relative1, relativeValue2 ]

        Relative3 relative1 relativeValue2 relativeValue3 ->
            [ relative1, relativeValue2, relativeValue3 ]

        Relative4 relative1 relativeValue2 relativeValue3 relativeValue4 ->
            [ relative1, relativeValue2, relativeValue3, relativeValue4 ]


applyToPadding : Relative -> Padding -> Padding
applyToPadding relativeValue base =
    let
        relatives =
            toList relativeValue

        paddings =
            Padding.toList base
    in
    List.map2 ((*) >> Size.map) relatives paddings
        |> Padding.fromList


applyToMargin : Relative -> Margin -> Margin
applyToMargin relativeValue base =
    let
        relatives =
            toList relativeValue

        paddings =
            Margin.toList base
    in
    List.map2 ((*) >> Size.map) relatives paddings
        |> Margin.fromList


applyToSize : Relative -> Size -> Size
applyToSize relativeValue base =
    Size.map ((*) (singular relativeValue)) base
