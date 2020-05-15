module Engage.Namespace exposing
    ( Namespace
    , engagecore
    , namespace
    , toString
    )


engagecore : Namespace
engagecore =
    namespace "engagecore-"


type Namespace
    = Namespace String


namespace : String -> Namespace
namespace name =
    Namespace name


toString : Namespace -> String
toString (Namespace value) =
    value
