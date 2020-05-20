module Engage.Custom.Field.Json exposing (encoder, fieldDecoder, fieldGroupDecoder, fileEntryDataEncoder, fileUploadEncoder)

{-| Custom.Field.Json

@docs encoder, fieldDecoder, fieldGroupDecoder, fileEntryDataEncoder, fileUploadEncoder

-}

import Dict
import Engage.Custom.Types exposing (BoolEntryData, Disable(..), EntryData, Field, FieldChoice, FieldGroup, FieldType(..), FileEntryData, FileStatus(..), MultipleEntryData, StaticFormType(..), UpdateOptions(..))
import Engage.Form.MembershipTypeList exposing (MembershipType)
import Engage.UI.Accordion as Accordion
import Engage.UI.Datepicker as Datepicker
import Engage.UI.Dropdown as Dropdown
import Engage.UI.Input as Input
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Set exposing (Set)


{-| The FieldGroup decoder
-}
fieldGroupDecoder : Date -> Decode.Decoder ( Int, FieldGroup )
fieldGroupDecoder now =
    let
        toFieldGroupDecoder : List ( Int, Field ) -> Int -> Decode.Decoder ( Int, FieldGroup )
        toFieldGroupDecoder fields relativeOrder =
            decode
                ( relativeOrder
                , { fieldGroupId = relativeOrder
                  , fields = Dict.fromList fields
                  , relativeOrder = relativeOrder
                  }
                )
    in
    decode toFieldGroupDecoder
        |> required "fields" (list (fieldTupleDecoder now))
        |> required "relativeOrder" int
        |> resolve


{-| The Field tuple decoder
-}
fieldTupleDecoder : Date -> Decode.Decoder ( Int, Field )
fieldTupleDecoder now =
    decode (\a b -> ( a, b ))
        |> required "fieldId" int
        |> custom (fieldDecoder now)


{-| The Field decoder
-}
fieldDecoder : Date -> Decode.Decoder Field
fieldDecoder now =
    decode Field
        |> required "fieldId" int
        |> required "relativeOrder" int
        |> required "label" string
        |> required "description" string
        |> custom (fieldTypeWithEntryDecoder now)
        |> required "required" bool
        |> optional "requiredErrorMessage" string ""
        |> required "disable" disableDecoder
        |> required "valueMin" string
        |> required "valueMax" string
        |> required "valueStep" string
        |> hardcoded AlwaysUpdate



-- |> required "entry" (maybe entryDecoder)


fieldTypeWithEntryDecoder : Date -> Decode.Decoder FieldType
fieldTypeWithEntryDecoder now =
    Decode.oneOf
        [ decode (entryDatafieldTypeDecoder now)
            |> required "entry" entryDataDecoder
            |> required "fieldType" string
            |> resolve
        , decode multipleEntryDataFieldTypeDecoder
            |> required "entry" multipleEntryDataDecoder
            |> required "fieldType" string
            |> resolve
        , decode boolEntryDataFieldTypeDecoder
            |> required "entry" boolEntryDataDecoder
            |> required "fieldType" string
            |> resolve
        , decode fileEntryDatafieldTypeDecoder
            |> required "entry" fileEntryDataDecoder
            |> required "fieldType" string
            |> resolve
        , decode noEntryFieldTypeDecoder
            |> required "fieldType" string
            |> resolve
        ]


noEntryFieldTypeDecoder : String -> Decode.Decoder FieldType
noEntryFieldTypeDecoder fieldType =
    case fieldType of
        "Text" ->
            Decode.succeed Text

        "Participant" ->
            Decode.succeed (StaticForm ParticipantForm)

        "MembershipTypeList" ->
            field "fieldChoices" (list membershipTypeDecoder)
                |> andThen
                    (\membershipTypes ->
                        Decode.succeed (StaticForm (MembershipTypeList { membershipTypeList = membershipTypes, state = Accordion.initialState, entry = Nothing }))
                    )

        _ ->
            Decode.fail ("Attempting to access invalid form field type: " ++ fieldType ++ ".")


entryDatafieldTypeDecoder : Date -> EntryData -> String -> Decode.Decoder FieldType
entryDatafieldTypeDecoder now entryData fieldType =
    case fieldType of
        "TextBox" ->
            TextBox
                { entry = entryData
                , state = Input.initialState
                }
                |> Decode.succeed

        "LargeTextBox" ->
            LargeTextBox
                { entry = entryData
                , state = Input.initialState
                }
                |> Decode.succeed

        "TextArea" ->
            TextArea
                { entry = entryData
                , state = Input.initialState
                }
                |> Decode.succeed

        "CheckBox" ->
            Decode.fail "Trying to decode 'CheckBox' type as entry data"

        "DropDown" ->
            field "fieldChoices" (list fieldChoiceDecoder)
                |> andThen
                    (\fieldChoices ->
                        DropDown
                            { entry = entryData
                            , state = Dropdown.initialState
                            , fieldChoices = fieldChoices
                            }
                            |> Decode.succeed
                    )

        "RadioList" ->
            field "fieldChoices" (list fieldChoiceDecoder)
                |> andThen
                    (\fieldChoices ->
                        RadioList
                            { entry = entryData
                            , state = Input.initialState
                            , fieldChoices = fieldChoices
                            }
                            |> Decode.succeed
                    )

        "CheckBoxList" ->
            Decode.fail "Trying to decode 'CheckBoxList' type as entry data instead of multiple entry data"

        "Quantity" ->
            Quantity
                { entry = entryData
                , state = Input.initialState
                }
                |> Decode.succeed

        "Date" ->
            Date
                { entry = entryData
                , state = Datepicker.initialState now
                }
                |> Decode.succeed

        "Email" ->
            Email
                |> Decode.succeed

        "Phone" ->
            Phone
                |> Decode.succeed

        "ZipCode" ->
            ZipCode
                |> Decode.succeed

        "USState" ->
            USState
                |> Decode.succeed

        "File" ->
            Decode.fail "Trying to decode 'File' type as entry data"

        "Country" ->
            Country
                { entry = entryData
                , state = Dropdown.initialState
                }
                |> Decode.succeed

        "Region" ->
            Region
                { entry = entryData
                , state = Dropdown.initialState
                }
                |> Decode.succeed

        "Text" ->
            Decode.fail "Trying to decode 'Text' type as entry data"

        _ ->
            Decode.fail ("Attempting to access invalid form field type: " ++ fieldType ++ ".")


multipleEntryDataFieldTypeDecoder : MultipleEntryData -> String -> Decode.Decoder FieldType
multipleEntryDataFieldTypeDecoder entryData fieldType =
    case fieldType of
        "CheckBoxList" ->
            field "fieldChoices" (list fieldChoiceDecoder)
                |> andThen
                    (\fieldChoices ->
                        CheckBoxList
                            { entry = entryData
                            , state = Input.initialState
                            , fieldChoices = fieldChoices
                            }
                            |> Decode.succeed
                    )

        _ ->
            Decode.fail ("Attempting to access invalid form field type: " ++ fieldType ++ ".")


boolEntryDataFieldTypeDecoder : BoolEntryData -> String -> Decode.Decoder FieldType
boolEntryDataFieldTypeDecoder entryData fieldType =
    case fieldType of
        "CheckBox" ->
            CheckBox
                { entry = entryData
                , state = Input.initialState
                }
                |> Decode.succeed

        _ ->
            Decode.fail ("Attempting to access invalid form field type: " ++ fieldType ++ ".")


fileEntryDatafieldTypeDecoder : FileEntryData -> String -> Decode.Decoder FieldType
fileEntryDatafieldTypeDecoder fileEntryData fieldType =
    case fieldType of
        "File" ->
            File
                { entry = fileEntryData
                , state = Input.initialState
                }
                |> Decode.succeed

        _ ->
            Decode.fail ("Attempting to access invalid form field type: " ++ fieldType ++ ".")


disableDecoder : Decode.Decoder Disable
disableDecoder =
    let
        toDisableDecoder : Int -> Decode.Decoder Disable
        toDisableDecoder value =
            case value of
                0 ->
                    decode None

                1 ->
                    decode Disabled

                2 ->
                    decode Hidden

                _ ->
                    Decode.fail ("Attempting to access invalid form disable value: " ++ toString value ++ ".")
    in
    int |> Decode.andThen toDisableDecoder


fieldChoiceDecoder : Decode.Decoder FieldChoice
fieldChoiceDecoder =
    decode FieldChoice
        |> required "fieldChoiceId" (maybe int)
        |> required "name" string
        |> required "value" string
        |> required "relativeOrder" int


membershipTypeDecoder : Decode.Decoder MembershipType
membershipTypeDecoder =
    decode MembershipType
        |> required "name" string
        |> required "value" int
        |> required "description" string
        |> required "price" string


entryDataDecoder : Decode.Decoder EntryData
entryDataDecoder =
    Decode.oneOf
        [ decode EntryData
            |> required "value" string
        , Decode.succeed
            { value = ""
            }
        ]


multipleEntryDataDecoder : Decode.Decoder MultipleEntryData
multipleEntryDataDecoder =
    Decode.oneOf
        [ decode MultipleEntryData
            |> required "values" (Decode.oneOf [ list string |> map Set.fromList, null Set.empty ])
        , null (MultipleEntryData Set.empty)
        ]


boolEntryDataDecoder : Decode.Decoder BoolEntryData
boolEntryDataDecoder =
    let
        boolStringDecoder =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toUpper str of
                            "TRUE" ->
                                Decode.succeed True

                            "FALSE" ->
                                Decode.succeed False

                            _ ->
                                Decode.fail (str ++ " is not a valid bool")
                    )
    in
    Decode.oneOf
        [ decode BoolEntryData
            |> required "value" bool
        , decode BoolEntryData
            |> required "value" boolStringDecoder
        , Decode.succeed
            { value = False
            }
        ]


fileEntryDataDecoder : Decode.Decoder FileEntryData
fileEntryDataDecoder =
    Decode.oneOf
        [ decode FileEntryData
            |> required "name" string
            |> required "fileType" string
            |> hardcoded Uploaded
        , Decode.succeed
            { name = ""
            , fileType = ""
            , status = NoFile
            }
        ]


{-| The file upload encoder
-}
fileUploadEncoder :
    { a
        | registrationId : Maybe Int
        , participantId : Int
        , fieldId : Int
        , formFilloutId : Maybe Int
        , domId : String
    }
    -> Encode.Value
fileUploadEncoder args =
    Encode.object
        [ ( "registrationId", Maybe.map Encode.int args.registrationId |> Maybe.withDefault Encode.null )
        , ( "participantId", Encode.int args.participantId )
        , ( "fieldId", Encode.int args.fieldId )
        , ( "formFilloutId", Maybe.map Encode.int args.formFilloutId |> Maybe.withDefault Encode.null )
        , ( "domId", Encode.string args.domId )
        ]


{-| The Field encoder
-}
encoder : Field -> Encode.Value
encoder field =
    Encode.object
        [ ( "fieldId", Encode.int field.fieldId )
        , entryEncoder field.fieldType
        ]


entryEncoder : FieldType -> ( String, Encode.Value )
entryEncoder fieldType =
    case fieldType of
        TextBox { entry } ->
            entryDataEncoder entry

        LargeTextBox { entry } ->
            entryDataEncoder entry

        TextArea { entry } ->
            entryDataEncoder entry

        CheckBox { entry } ->
            boolEntryDataEncoder entry

        DropDown { entry } ->
            entryDataEncoder entry

        RadioList { entry } ->
            entryDataEncoder entry

        CheckBoxList { entry } ->
            multipleEntryDataEncoder entry

        Quantity { entry } ->
            entryDataEncoder entry

        Date { entry } ->
            entryDataEncoder entry

        Email ->
            ( "entry", Encode.null )

        Phone ->
            ( "entry", Encode.null )

        ZipCode ->
            ( "entry", Encode.null )

        USState ->
            ( "entry", Encode.null )

        File { entry } ->
            fileEntryDataEncoder entry

        Country { entry } ->
            entryDataEncoder entry

        Region { entry } ->
            entryDataEncoder entry

        Text ->
            ( "entry", Encode.null )

        StaticForm (MembershipTypeList data) ->
            membershipTypeEncoder data

        StaticForm ParticipantForm ->
            ( "entry", Encode.null )


membershipTypeEncoder : { state : Accordion.State, membershipTypeList : List MembershipType, entry : Maybe MembershipType } -> ( String, Encode.Value )
membershipTypeEncoder data =
    ( "membershipType"
    , data.entry
        |> Maybe.map (.value >> Encode.int)
        |> Maybe.withDefault Encode.null
    )


entryDataEncoder : EntryData -> ( String, Encode.Value )
entryDataEncoder entryData =
    ( "entry"
    , Encode.object
        [ ( "value", Encode.string entryData.value )
        ]
    )


multipleEntryDataEncoder : MultipleEntryData -> ( String, Encode.Value )
multipleEntryDataEncoder multipleEntryData =
    ( "multipleEntry"
    , Encode.object
        [ ( "values", Encode.list <| List.map Encode.string <| Set.toList <| multipleEntryData.values )
        ]
    )


boolEntryDataEncoder : BoolEntryData -> ( String, Encode.Value )
boolEntryDataEncoder entryData =
    ( "entry"
    , Encode.object
        [ ( "value", Encode.bool entryData.value )
        ]
    )


{-| The FieldEntryData encoder
-}
fileEntryDataEncoder : FileEntryData -> ( String, Encode.Value )
fileEntryDataEncoder fileEntryData =
    ( "fileEntry"
    , Encode.object
        [ ( "name", Encode.string fileEntryData.name )
        , ( "type", Encode.string fileEntryData.fileType )
        ]
    )
