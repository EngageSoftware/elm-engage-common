module Engage.Entity.Gender exposing
    ( Gender(..)
    , decoder
    , encoder
    , fromString
    , toString
    )

import Json.Decode as JD
import Json.Encode as JE


type Gender
    = Male
    | Female
    | Other
    | Unspecified


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


decoder : JD.Decoder Gender
decoder =
    JD.nullable JD.string |> JD.map (Maybe.map fromString >> Maybe.withDefault Unspecified)


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
