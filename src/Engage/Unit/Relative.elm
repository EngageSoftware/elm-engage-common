module Engage.Unit.Relative exposing (Relative(..), applyToMargin, applyToPadding, applyToSize, relative, relative2, relative3, relative4, singular, toList)

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
singular relative =
    case relative of
        Relative relative ->
            relative

        Relative2 relative _ ->
            relative

        Relative3 relative _ _ ->
            relative

        Relative4 relative _ _ _ ->
            relative


toList : Relative -> List Float
toList relative =
    case relative of
        Relative relative ->
            [ relative ]

        Relative2 relative1 relative2 ->
            [ relative1, relative2 ]

        Relative3 relative1 relative2 relative3 ->
            [ relative1, relative2, relative3 ]

        Relative4 relative1 relative2 relative3 relative4 ->
            [ relative1, relative2, relative3, relative4 ]


applyToPadding : Relative -> Padding -> Padding
applyToPadding relative base =
    let
        relatives =
            toList relative

        paddings =
            Padding.toList base
    in
    List.map2 ((*) >> Size.map) relatives paddings
        |> Padding.fromList


applyToMargin : Relative -> Margin -> Margin
applyToMargin relative base =
    let
        relatives =
            toList relative

        paddings =
            Margin.toList base
    in
    List.map2 ((*) >> Size.map) relatives paddings
        |> Margin.fromList


applyToSize : Relative -> Size -> Size
applyToSize relative base =
    Size.map ((*) (singular relative)) base
