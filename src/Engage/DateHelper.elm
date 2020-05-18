module Engage.DateHelper exposing (toDateIgnoreTimezone, toUTC)

{-| Data helpers

@docs toDateIgnoreTimezone

@docs toUTC

-}

import Date exposing (Date)
import Date.Extra.Create
import Time


{-| Convert a date to a UTC date
-}
toUTC : Date -> Date
toUTC date =
    let
        offsetInMinutes =
            Date.Extra.Create.getTimezoneOffset date
    in
    Date.fromTime (Date.toTime date + toFloat (offsetInMinutes * 60000))


{-| Convert a time to a date
-}
toDateIgnoreTimezone : Time.Time -> Date
toDateIgnoreTimezone time =
    Date.fromTime time
        |> toUTC
