module Engage.Bool exposing (false, true)


{-| Bool helpers

@docs true

@docs false

-}

type FancyBool a
    = Nothing
    | Just a


{-| Get a FancyBool from a Bool value
-}
true : a -> Bool -> FancyBool a
true a bool =
    if bool then
        Just a

    else
        Nothing


{-| Get a Bool value from FancyBool
-}
false : a -> FancyBool a -> a
false default fancyBool =
    case fancyBool of
        Nothing ->
            default

        Just a ->
            a
