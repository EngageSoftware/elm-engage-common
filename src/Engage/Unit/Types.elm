module Engage.Unit.Types exposing (Compatible(..), ExplicitLength, Style(..))


type alias ExplicitLength units =
    { value : String
    , numericValue : Float
    , units : units
    , unitLabel : String
    , length : Compatible
    , lengthOrAuto : Compatible
    , lengthOrNumber : Compatible
    , lengthOrNone : Compatible
    , lengthOrMinMaxDimension : Compatible
    , lengthOrNoneOrMinMaxDimension : Compatible
    , textIndent : Compatible
    , flexBasis : Compatible
    , lengthOrNumberOrAutoOrNoneOrContent : Compatible
    , fontSize : Compatible
    , lengthOrAutoOrCoverOrContain : Compatible
    }


type Compatible
    = Compatible


type Style a
    = Style a
