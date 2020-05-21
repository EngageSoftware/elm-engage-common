module Engage.Entity.Participant exposing
    ( Participant
    , decoder, empty, encoder, encoderWith, toParticipant
    )

{-| Entity.Participant

@docs Participant

@docs decoder, empty, encoder, encoderWith, toParticipant

-}

import Date exposing (Date)
import Engage.Entity.Account as Account exposing (Account)
import Engage.Entity.Address as Address exposing (Address)
import Engage.Entity.Gender as Gender exposing (Gender)
import Engage.Entity.PhoneNumber as PhoneNumber exposing (PhoneNumber)
import Engage.ListItem as ListItem exposing (ListItem)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Time


{-| The Participant type
-}
type alias Participant =
    { participantId : Maybe Int
    , firstName : String
    , lastName : String
    , middleName : String
    , email : String
    , primaryAddress : Maybe Address
    , phone : PhoneNumber
    , mobilePhone : PhoneNumber
    , profilePicture : String
    , gender : Gender
    , birthDate : Maybe Date
    , birthDateYear : Maybe ListItem
    , birthDateMonth : Maybe ListItem
    , account : Maybe Account
    }


{-| Get an empty Participant
-}
empty : Participant
empty =
    { participantId = Nothing
    , firstName = ""
    , lastName = ""
    , middleName = ""
    , email = ""
    , primaryAddress = Nothing
    , phone = PhoneNumber.empty
    , mobilePhone = PhoneNumber.empty
    , profilePicture = ""
    , gender = Gender.Unspecified
    , birthDate = Nothing
    , birthDateYear = Nothing
    , birthDateMonth = Nothing
    , account = Nothing
    }


type alias ParticipantLike a =
    { a
        | participantId : Maybe Int
        , firstName : String
        , lastName : String
        , middleName : String
        , email : String
        , primaryAddress : Maybe Address
        , phone : PhoneNumber
        , mobilePhone : PhoneNumber
        , profilePicture : String
        , gender : Gender
        , birthDate : Maybe Date
        , birthDateYear : Maybe ListItem
        , birthDateMonth : Maybe ListItem
        , account : Maybe Account
    }


{-| Get a Participant from a partial
-}
toParticipant : ParticipantLike a -> Participant
toParticipant data =
    { participantId = data.participantId
    , firstName = data.firstName
    , lastName = data.lastName
    , middleName = data.middleName
    , email = data.email
    , primaryAddress = data.primaryAddress
    , phone = data.phone
    , mobilePhone = data.mobilePhone
    , profilePicture = data.profilePicture
    , gender = data.gender
    , birthDate = data.birthDate
    , birthDateYear = data.birthDateYear
    , birthDateMonth = data.birthDateMonth
    , account = data.account
    }


{-| A Participant decoder
-}
decoder : Decoder Participant
decoder =
    JD.succeed Participant
        |> JDP.required "participantId" (JD.map Just JD.int)
        |> JDP.required "firstName" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "lastName" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "middleName" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "email" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "primaryAddress" (JD.maybe Address.decoder)
        |> JDP.required "phone" PhoneNumber.decoder
        |> JDP.required "mobilePhone" PhoneNumber.decoder
        |> JDP.required "profilePicture" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "gender" Gender.decoder
        |> JDP.required "birthDatePosix" (JD.nullable (JD.int |> JD.map ((*) 1000 >> Time.millisToPosix >> Date.fromPosix Time.utc)))
        |> JDP.required "birthDateYear" (JD.maybe (JD.int |> JD.map (\val -> ( val, "" ))))
        |> JDP.required "birthDateMonth" (JD.maybe (JD.int |> JD.map (\val -> ( val, "" ))))
        |> JDP.required "account" (JD.maybe Account.decoder)


{-| A Participant encoder
-}
encoder : ParticipantLike a -> JE.Value
encoder =
    encoderWith []


{-| A Participant with fields encoder
-}
encoderWith : List ( String, JE.Value ) -> ParticipantLike a -> JE.Value
encoderWith fields participantData =
    JE.object
        (fields
            ++ [ ( "participantId", Maybe.map JE.int participantData.participantId |> Maybe.withDefault JE.null )
               , ( "firstName", JE.string participantData.firstName )
               , ( "middleName", JE.string participantData.middleName )
               , ( "lastName", JE.string participantData.lastName )
               , ( "email", JE.string participantData.email )
               , ( "phone", PhoneNumber.encoder participantData.phone )
               , ( "mobilePhone", PhoneNumber.encoder participantData.mobilePhone )
               , ( "profilePicture", JE.string participantData.profilePicture )
               , ( "gender", Gender.encoder participantData.gender )
               , ( "birthDatePosix", Maybe.map (Tuple.first >> JE.int) participantData.birthDateYear |> Maybe.withDefault JE.null )
               , ( "birthDateYear", Maybe.map (Tuple.first >> JE.int) participantData.birthDateYear |> Maybe.withDefault JE.null )
               , ( "birthDateMonth", Maybe.map (Tuple.first >> JE.int) participantData.birthDateMonth |> Maybe.withDefault JE.null )
               , ( "account", Maybe.map Account.encoder participantData.account |> Maybe.withDefault JE.null )
               , ( "primaryAddress", Maybe.map Address.encoder participantData.primaryAddress |> Maybe.withDefault JE.null )
               ]
        )
