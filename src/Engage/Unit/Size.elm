module Engage.Unit.Size exposing (Size(..), auto, em, fontSize, map, notSet, px, rem, toCss, zero)

import Css


type Size
    = NotSet
    | Em Float
    | Rem Float
    | Px Float
    | SizeAuto


map : (Float -> Float) -> Size -> Size
map f size =
    case size of
        SizeAuto ->
            SizeAuto

        NotSet ->
            NotSet

        Em number ->
            em (f number)

        Rem number ->
            rem (f number)

        Px number ->
            px (f number)


toCss : Size -> String
toCss size =
    case size of
        SizeAuto ->
            "auto"

        NotSet ->
            ""

        Em number ->
            toString number ++ "em"

        Rem number ->
            toString number ++ "rem"

        Px number ->
            toString number ++ "px"


fontSize : Size -> Css.Mixin
fontSize size =
    if size == NotSet then
        Css.mixin []

    else
        Css.property "font-size" (toCss size)


em : Float -> Size
em number =
    Em number


rem : Float -> Size
rem number =
    Rem number


px : Float -> Size
px number =
    Px number


notSet : Size
notSet =
    NotSet


auto : Size
auto =
    SizeAuto


zero : Size
zero =
    Px 0
