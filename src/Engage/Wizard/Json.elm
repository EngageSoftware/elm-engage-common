module Engage.Wizard.Json exposing
    ( RegistrationInfo
    , WizardResponse(..)
    , postStepEncoder
    , wizardDataDecoder
    , wizardResponseDecoder
    , wizardStepEncoder
    )

import Date exposing (Date)
import Dict
import Engage.Custom.Form.Json as CustomForm
import Engage.Custom.Types as Custom
import Engage.Entity.Account as Account exposing (Account)
import Engage.Entity.Address as Address
import Engage.Entity.Participant as Participant exposing (Participant)
import Engage.Form.Company as Company
import Engage.Form.Company.Json as Company
import Engage.Form.Company.Types as Company
import Engage.Form.Participant as Participant
import Engage.RemoteData
import Engage.Wizard exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Engage.SelectDict exposing (SelectDict)


{-| Preparing for when we need to handle in the case the user have multiple pending registration.
-}
type WizardResponse
    = OkResponse WizardData
    | MultipleRegistration (List RegistrationInfo)


type alias RegistrationInfo =
    { registrationId : Int
    , info : String
    }


wizardResponseDecoder : Date -> Decode.Decoder WizardResponse
wizardResponseDecoder date =
    Decode.oneOf
        [ wizardDataDecoder date |> Decode.map OkResponse
        ]


wizardDataDecoder : Date -> Decode.Decoder WizardData
wizardDataDecoder now =
    let
        toWizardData : Maybe Participant -> Maybe Account -> Maybe (Company.CompaniesData {}) -> List ( Int, Step ) -> Maybe Int -> Maybe Int -> Decode.Decoder WizardData
        toWizardData participantData invitedAccount companiesData steps registrationId membershipEventId =
            let
                selectDict : Result String (SelectDict Int Step)
                selectDict =
                    convertToSelectDict steps
            in
            case selectDict of
                Result.Err err ->
                    Decode.fail err

                Result.Ok steps ->
                    Decode.succeed
                        { participant = participantData
                        , invitedAccount = invitedAccount
                        , steps = steps
                        , participantCompanies = companiesData
                        , registrationId = registrationId
                        , membershipEventId = membershipEventId
                        }

        listStepDecoder : Decode.Decoder (List ( Int, Step ))
        listStepDecoder =
            wizardStepTupleDecoder now |> list

        participantDecoder =
            Participant.decoder
                |> Decode.map (\participant -> { participant | primaryAddress = participant.primaryAddress |> Maybe.withDefault Address.emptyPrimaryAddress |> Just })
    in
    decode toWizardData
        |> optional "participant" (Decode.maybe participantDecoder) Nothing
        |> optional "invitedAccount" (Decode.maybe Account.decoder) Nothing
        |> optional "participantCompanies" (Decode.maybe Company.decoder) Nothing
        |> required "steps" listStepDecoder
        |> optional "registrationId" (Decode.maybe Decode.int) Nothing
        |> optional "membershipEventId" (Decode.maybe Decode.int) Nothing
        |> resolve
        |> map updateMembershipEventId


updateMembershipEventId : WizardData -> WizardData
updateMembershipEventId wizardData =
    case wizardData.membershipEventId of
        Nothing ->
            wizardData

        Just membershipEventId ->
            { wizardData | steps = SelectDict.map (\_ step -> Engage.Wizard.updateMembershipEventId membershipEventId step) wizardData.steps }


wizardStepTupleDecoder : Date -> Decode.Decoder ( Int, Step )
wizardStepTupleDecoder now =
    decode (,)
        |> required "relativeOrder" int
        |> custom (wizardStepDecoder now)


wizardStepDecoder : Date -> Decode.Decoder Step
wizardStepDecoder now =
    decode (wizardStepTypeDecoder now)
        |> required "type" int
        |> resolve


wizardStepTypeDecoder : Date -> Int -> Decode.Decoder Step
wizardStepTypeDecoder now typeId =
    case typeId of
        1 ->
            {- Information -}
            infoStepDecoder

        2 ->
            {- MultiForm -}
            formsStepDecoder now

        3 ->
            {- EventForm -}
            formsStepDecoder now

        _ ->
            Decode.fail "Attempting to load an invalid wizard step."


infoStepDecoder : Decode.Decoder Step
infoStepDecoder =
    let
        toInfoStepDecoder : Int -> String -> String -> Decode.Decoder Step
        toInfoStepDecoder id name info =
            Info
                { id = id
                , name = name
                , info = info
                , stepResponse = Engage.RemoteData.NotAsked
                }
                |> decode
    in
    decode toInfoStepDecoder
        |> required "relativeOrder" int
        |> required "name" string
        |> required "information" string
        |> resolve


formsStepDecoder : Date -> Decode.Decoder Step
formsStepDecoder now =
    let
        toFormsStepDecoder : Int -> String -> List ( Int, Form ) -> Decode.Decoder Step
        toFormsStepDecoder id name forms =
            case convertToSelectDict forms of
                Result.Err err ->
                    Decode.fail err

                Result.Ok forms ->
                    Forms
                        { id = id
                        , name = name
                        , forms = forms
                        , stepResponse = Engage.RemoteData.NotAsked
                        }
                        |> decode
    in
    decode toFormsStepDecoder
        |> required "relativeOrder" int
        |> required "name" string
        |> required "forms" (list (wizardFormTupleDecoder now))
        |> resolve


wizardFormTupleDecoder : Date -> Decode.Decoder ( Int, Form )
wizardFormTupleDecoder now =
    decode (,)
        |> required "relativeOrder" int
        |> custom (wizardFormDecoder now)


wizardFormDecoder : Date -> Decode.Decoder Form
wizardFormDecoder now =
    decode (wizardFormTypeDecoder now)
        |> required "type" int
        |> required "relativeOrder" int
        |> optionalAt [ "form", "title" ] string ""
        |> optionalAt [ "form", "description" ] string ""
        |> resolve


wizardFormTypeDecoder : Date -> Int -> Int -> String -> String -> Decode.Decoder Form
wizardFormTypeDecoder now formType relativeOrder name description =
    case formType of
        1 ->
            {- Participant -}
            if description == "default participant" || String.isEmpty name then
                participantFormDecoder now

            else
                customFormDecoder now Custom.Participant relativeOrder

        2 ->
            {- MultiForm -}
            customFormDecoder now Custom.Additional relativeOrder

        3 ->
            {- Registration -}
            customFormDecoder now Custom.Registration relativeOrder

        4 ->
            {- Company -}
            if description == "default company" || String.isEmpty name then
                companiesFormDecoder now

            else
                customFormDecoder now Custom.Company relativeOrder

        _ ->
            Decode.fail "Attempting to load an invalid form."


participantFormDecoder : Date -> Decode.Decoder Form
participantFormDecoder now =
    let
        toParticipantFormDecoder : Int -> String -> Decode.Decoder Form
        toParticipantFormDecoder id name =
            Participant.emptyForm id now name
                |> ParticipantForm
                |> decode
    in
    decode toParticipantFormDecoder
        |> required "relativeOrder" int
        |> optionalAt [ "form", "title" ] string ""
        |> resolve


customFormDecoder : Date -> Custom.Level -> Int -> Decode.Decoder Form
customFormDecoder now formLevel relativeOrder =
    let
        toCustomFormDecoder : Int -> String -> Custom.Form -> Decode.Decoder Form
        toCustomFormDecoder id name customForm =
            CustomForm
                { id = id
                , name = name
                , form = customForm
                , stepResponse = Engage.RemoteData.NotAsked
                }
                |> decode
    in
    decode toCustomFormDecoder
        |> required "relativeOrder" int
        |> requiredAt [ "form", "title" ] string
        |> required "form" (CustomForm.decoder now formLevel relativeOrder)
        |> resolve


companiesFormDecoder : Date -> Decode.Decoder Form
companiesFormDecoder now =
    let
        toCompaniesFormDecoder : Int -> String -> Decode.Decoder Form
        toCompaniesFormDecoder id name =
            Company.emptyForm id now name
                |> CompaniesForm
                |> decode
    in
    decode toCompaniesFormDecoder
        |> required "relativeOrder" int
        |> optionalAt [ "form", "title" ] string ""
        |> resolve


convertToSelectDict : List ( comparable, data ) -> Result String (SelectDict comparable data)
convertToSelectDict unsortedList =
    let
        sortedList =
            List.sortBy Tuple.first unsortedList

        maybeHead =
            sortedList
                |> List.head

        tail =
            List.tail sortedList
    in
    case maybeHead of
        Just head ->
            Result.Ok
                (SelectDict.fromDicts
                    Dict.empty
                    head
                    (case tail of
                        Nothing ->
                            Dict.empty

                        Just remainder ->
                            Dict.fromList remainder
                    )
                )

        Nothing ->
            Result.Err "Empty list"


postStepEncoder : { a | membershipConfigurationId : Int, participantId : Maybe Int, registrationId : Maybe Int, step : Step } -> Encode.Value
postStepEncoder { membershipConfigurationId, participantId, registrationId, step } =
    wizardStepEncoder
        [ ( "membershipConfigurationId", Encode.int membershipConfigurationId )
        , ( "participantId", Maybe.map Encode.int participantId |> Maybe.withDefault Encode.null )
        , ( "registrationId", Maybe.map Encode.int registrationId |> Maybe.withDefault Encode.null )
        ]
        step


wizardStepEncoder : List ( String, Encode.Value ) -> Step -> Encode.Value
wizardStepEncoder encoders step =
    case step of
        Info stepData ->
            Encode.object (encoders ++ [])

        Forms stepData ->
            Encode.object (encoders ++ [])
