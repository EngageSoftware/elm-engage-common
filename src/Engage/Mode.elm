module Engage.Mode exposing
    ( Mode(..)
    , decoder
    , fromBool
    , isReactor
    )

{-| Mode helpers

@docs Mode

@docs fromBool

@docs isReactor

@docs decoder

-}

import Json.Decode as JD

{-| A mode that the application is running
-}
type Mode
    = Reactor
    | Development
    | Production


{-| Convert a bool to a Mode
-}
fromBool : Bool -> Mode
fromBool isDevelopment =
    if isDevelopment then
        Development

    else
        Production


{-| Check if the mode is reactor
-}
isReactor : Mode -> Bool
isReactor mode =
    case mode of
        Reactor ->
            True

        Development ->
            False

        Production ->
            False


{-| Mode decoder
-}
decoder : JD.Decoder Mode
decoder =
    JD.bool |> JD.map fromBool
