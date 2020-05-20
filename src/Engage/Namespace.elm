module Engage.Namespace exposing
    ( Namespace
    , engagecore, namespace, toString
    )

{-| Namespace

@docs Namespace

@docs engagecore, namespace, toString

-}


{-| The engagecore Namespace
-}
engagecore : Namespace
engagecore =
    namespace "engagecore-"


{-| A Namespace type
-}
type Namespace
    = Namespace String


{-| Get a Namespace from a String
-}
namespace : String -> Namespace
namespace name =
    Namespace name


{-| Get a String from a Namespace
-}
toString : Namespace -> String
toString (Namespace value) =
    value
