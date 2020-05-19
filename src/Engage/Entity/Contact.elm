module Engage.Entity.Contact exposing
    ( Contact
    , ContactType
    , ContactTypes
    , contactTypeDecoder
    , contactTypesDecoder
    , decoder
    , empty
    , encoder
    , encoderWith
    )

{-| Entity.Contact

@docs Contact, ContactType, ContactTypes

@docs contactTypeDecoder, contactTypesDecoder, decoder, empty, encoder, encoderWith

-}

import Dict exposing (Dict)
import Engage.Entity.PhoneNumber as PhoneNumber exposing (PhoneNumber)
import Engage.ListItem as ListItem exposing (ListItem)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE


{-| The ContactTypes type
-}
type alias ContactTypes =
    Dict Int ContactType


{-| The ContactType type
-}
type alias ContactType =
    { contactTypeId : Int
    , shortDescription : String
    , longDescription : String
    }


{-| The Contact type
-}
type alias Contact =
    { contactType : Maybe ContactType
    , contactId : Maybe Int
    , prefix : String
    , firstName : String
    , middleName : String
    , lastName : String
    , suffix : String
    , addressName : String
    , address1 : String
    , address2 : String
    , city : String
    , postalCode : String
    , region : Maybe ListItem
    , country : Maybe ListItem
    , phone : PhoneNumber
    , mobilePhone : PhoneNumber
    , fax : PhoneNumber
    , email : String
    , notes : String
    , isPrimaryContact : Bool
    , isBillingContact : Bool
    , relativeOrder : Int
    }


{-| Get an empty Contact
-}
empty : Contact
empty =
    { contactType = Nothing
    , contactId = Nothing
    , prefix = ""
    , firstName = ""
    , middleName = ""
    , lastName = ""
    , suffix = ""
    , addressName = ""
    , address1 = ""
    , address2 = ""
    , city = ""
    , postalCode = ""
    , region = Nothing
    , country = Nothing
    , phone = PhoneNumber.empty
    , mobilePhone = PhoneNumber.empty
    , fax = PhoneNumber.empty
    , email = ""
    , notes = ""
    , isPrimaryContact = False
    , isBillingContact = False
    , relativeOrder = 0
    }


{-| The Contact decoder
-}
decoder : Decoder Contact
decoder =
    JDP.decode Contact
        |> JDP.required "contactType" (JD.map Just contactTypeDecoder)
        |> JDP.required "contactId" (JD.nullable JD.int)
        |> JDP.required "prefix" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "firstName" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "middleName" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "lastName" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "suffix" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "addressName" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "address1" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "address2" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "city" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "postalCode" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "region" (JD.maybe ListItem.decoder)
        |> JDP.required "country" (JD.maybe ListItem.decoder)
        |> JDP.required "phone" PhoneNumber.decoder
        |> JDP.required "mobilePhone" PhoneNumber.decoder
        |> JDP.required "fax" PhoneNumber.decoder
        |> JDP.required "email" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "notes" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "isPrimaryContact" JD.bool
        |> JDP.required "isBillingContact" JD.bool
        |> JDP.required "relativeOrder" JD.int


{-| The ContactType decoder
-}
contactTypeDecoder : Decoder ContactType
contactTypeDecoder =
    JDP.decode ContactType
        |> JDP.required "contactTypeId" JD.int
        |> JDP.required "shortDescription" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "longDescription" (JD.oneOf [ JD.string, JD.null "" ])


{-| The ContactTypes decoder
-}
contactTypesDecoder : Decoder ContactTypes
contactTypesDecoder =
    JD.list contactTypeDecoder
        |> JD.map (List.map (\({ contactTypeId } as contactType) -> ( contactTypeId, contactType )))
        |> JD.map Dict.fromList


{-| The Contact encoder
-}
encoder : Contact -> JE.Value
encoder =
    encoderWith []


{-| The Contact encoder with values
-}
encoderWith : List ( String, JE.Value ) -> Contact -> JE.Value
encoderWith fields contactData =
    JE.object
        (fields
            ++ [ ( "contactTypeId", Maybe.map (.contactTypeId >> JE.int) contactData.contactType |> Maybe.withDefault JE.null )
               , ( "contactId", contactData.contactId |> Maybe.map JE.int |> Maybe.withDefault JE.null )
               , ( "prefix", JE.string contactData.prefix )
               , ( "firstName", JE.string contactData.firstName )
               , ( "middleName", JE.string contactData.middleName )
               , ( "lastName", JE.string contactData.lastName )
               , ( "suffix", JE.string contactData.suffix )
               , ( "email", JE.string contactData.email )
               , ( "addressName", JE.string contactData.addressName )
               , ( "address1", JE.string contactData.address1 )
               , ( "address2", JE.string contactData.address2 )
               , ( "city", JE.string contactData.city )
               , ( "regionId", Maybe.map (Tuple.first >> JE.int) contactData.region |> Maybe.withDefault JE.null )
               , ( "countryId", Maybe.map (Tuple.first >> JE.int) contactData.country |> Maybe.withDefault JE.null )
               , ( "postalCode", JE.string contactData.postalCode )
               , ( "phone", PhoneNumber.encoder contactData.phone )
               , ( "mobilePhone", PhoneNumber.encoder contactData.mobilePhone )
               , ( "fax", PhoneNumber.encoder contactData.fax )
               , ( "notes", JE.string contactData.notes )
               , ( "isPrimaryContact", JE.bool contactData.isPrimaryContact )
               , ( "isBillingContact", JE.bool contactData.isBillingContact )
               ]
        )
