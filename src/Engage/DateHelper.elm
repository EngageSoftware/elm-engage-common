module Engage.DateHelper exposing (toDateIgnoreTimezone, toUTC)

import Date exposing (Date)
import Date.Extra.Create
import Time


toUTC : Date -> Date
toUTC date =
    let
        offsetInMinutes =
            Date.Extra.Create.getTimezoneOffset date
    in
    Date.fromTime (Date.toTime date + toFloat (offsetInMinutes * 60000))


toDateIgnoreTimezone : Time.Time -> Date
toDateIgnoreTimezone time =
    Date.fromTime time
        |> toUTC
