module Engage.Mode exposing
    ( Mode(..)
    , decoder
    , fromBool
    , isReactor
    )

import Json.Decode as JD


type Mode
    = Reactor
    | Development
    | Production


fromBool : Bool -> Mode
fromBool isDevelopment =
    if isDevelopment then
        Development

    else
        Production


isReactor : Mode -> Bool
isReactor mode =
    case mode of
        Reactor ->
            True

        Development ->
            False

        Production ->
            False


decoder : JD.Decoder Mode
decoder =
    JD.bool |> JD.map fromBool
