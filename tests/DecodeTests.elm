module DecodeTests exposing (suite)

import Date
import Engage.Decode exposing (isoDateDecoder)
import Expect
import Json.Decode as Decode
import Test exposing (Test, describe, test)
import Time exposing (Month(..))


suite : Test
suite =
    describe "Engage.Decode"
        [ describe "isoDateDecoder"
            [ describe "fails"
                [ test "Fails with invalid date" <|
                    \_ ->
                        Decode.decodeString isoDateDecoder """
                "0000-00-00"
                """
                            |> Expect.err
                , test "Fails with empty" <|
                    \_ ->
                        Decode.decodeString isoDateDecoder """
                ""
                """
                            |> Expect.err
                ]
            , test "Decodes ISO date" <|
                \_ ->
                    Decode.decodeString isoDateDecoder """
                "2026-04-30"
                """
                        |> Expect.equal (Ok (Date.fromCalendarDate 2026 Apr 30))
            , test "Decodes ISO date/time" <|
                \_ ->
                    Decode.decodeString isoDateDecoder """
                "2025-05-27T10:21:00.607"
                """
                        |> Expect.equal (Ok (Date.fromCalendarDate 2025 May 27))
            ]
        ]
