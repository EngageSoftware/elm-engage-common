module Engage.Custom.Form.Json exposing
    ( decoder
    , encoder
    )

import Date exposing (Date)
import Dict
import Engage.Custom.Field.Json as CustomField
import Engage.Custom.Section as CustomSection exposing (sectionTupleDecoder)
import Engage.Custom.Types exposing (Form, Level)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode as Encode


encoder : Form -> Encode.Value
encoder form =
    let
        fields =
            form.sections
                |> Dict.values
                |> List.concatMap (.fieldGroups >> Dict.values)
                |> List.concatMap (.fields >> Dict.values)
    in
    Encode.object
        [ ( "formId", Encode.int form.formId )
        , ( "formFilloutId", Maybe.map Encode.int form.formFilloutId |> Maybe.withDefault Encode.null )
        , ( "fields", fields |> List.map CustomField.encoder |> Encode.list )
        ]


decoder : Date -> Level -> Int -> Decode.Decoder Form
decoder now formLevel relativeOrder =
    decode Form
        |> required "formId" int
        |> required "formFilloutId" (Decode.maybe int)
        |> required "title" string
        |> required "formSections"
            (list (sectionTupleDecoder now)
                |> Decode.andThen (Dict.fromList >> decode)
            )
        |> hardcoded formLevel
        |> hardcoded relativeOrder
        |> hardcoded []
