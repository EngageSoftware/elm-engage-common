module Bool exposing (false, true)


type FancyBool a
    = Nothing
    | Just a


true : a -> Bool -> FancyBool a
true a bool =
    if bool then
        Just a

    else
        Nothing


false : a -> FancyBool a -> a
false default fancyBool =
    case fancyBool of
        Nothing ->
            default

        Just a ->
            a
