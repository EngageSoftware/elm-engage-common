module Engage.Entity.Gender exposing
    ( Gender(..)
    , decoder, encoder, fromString, toString
    )

{-| Entity.Gender

@docs Gender

@docs decoder, encoder, fromString, toString

-}

import Json.Decode as JD
import Json.Encode as JE


{-| The Gender type
-}
type Gender
    = Male
    | Female
    | Other
    | Unspecified


{-| The Gender encoder
-}
encoder : Gender -> JE.Value
encoder gender =
    case gender of
        Male ->
            JE.string "Male"

        Female ->
            JE.string "Female"

        Other ->
            JE.string "Other"

        Unspecified ->
            JE.null


{-| The Gender decoder
-}
decoder : JD.Decoder Gender
decoder =
    JD.nullable JD.string |> JD.map (Maybe.map fromString >> Maybe.withDefault Unspecified)


{-| Convert a String to a Gender
-}
fromString : String -> Gender
fromString value =
    case String.toUpper <| String.trim <| value of
        "MALE" ->
            Male

        "FEMALE" ->
            Female

        "" ->
            Unspecified

        _ ->
            Other


{-| Convert a Gender to a String
-}
toString : Gender -> String
toString gender =
    case gender of
        Male ->
            "Male"

        Female ->
            "Female"

        Other ->
            "Other"

        Unspecified ->
            ""
