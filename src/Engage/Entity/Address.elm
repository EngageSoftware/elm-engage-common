module Engage.Entity.Address exposing
    ( Address, AddressLike, AddressType, AddressTypes, Countries, CountryId, RegionId, Regions, RegionsCountry
    , addressTypeDecoder, addressTypesDecoder, countriesDecoder, decoder, empty, emptyAddressType, emptyPrimaryAddress, encoder, encoderWith, getRegionsForCountry, regionsCountryDecoder
    )

{-| Entity.Address

@docs Address, AddressLike, AddressType, AddressTypes, Countries, CountryId, RegionId, Regions, RegionsCountry

@docs addressTypeDecoder, addressTypesDecoder, countriesDecoder, decoder, empty, emptyAddressType, emptyPrimaryAddress, encoder, encoderWith, getRegionsForCountry, regionsCountryDecoder

-}

import Dict exposing (Dict)
import Engage.Entity.PhoneNumber as PhoneNumber exposing (PhoneNumber)
import Engage.ListItem as ListItem exposing (ListItem)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE


{-| The CountryId type
-}
type alias CountryId =
    Int


{-| The RegionId type
-}
type alias RegionId =
    Int


{-| The AddressTypeId type
-}
type alias AddressTypeId =
    Int


{-| The Countries type
-}
type alias Countries =
    Dict CountryId CountryData


{-| The CountryData type
-}
type alias CountryData =
    { countryId : Int, countryName : String, countryIsoCode : String }


{-| The RegionsCountry type
-}
type alias RegionsCountry =
    Dict CountryId Regions


{-| The Regions type
-}
type alias Regions =
    Dict RegionId RegionData


{-| The AddressTypes type
-}
type alias AddressTypes =
    Dict AddressTypeId AddressType


{-| The RegionData type
-}
type alias RegionData =
    { regionId : Int, regionName : String }


{-| The AddressType type
-}
type alias AddressType =
    { addressTypeId : Int
    , shortDescription : String
    , longDescription : String
    }


{-| Get an empty AddressType
-}
emptyAddressType : AddressType
emptyAddressType =
    { addressTypeId = 0
    , shortDescription = ""
    , longDescription = ""
    }


{-| An AddressType decoder
-}
addressTypeDecoder : Decoder AddressType
addressTypeDecoder =
    JD.succeed AddressType
        |> JDP.required "addressTypeId" JD.int
        |> JDP.required "shortDescription" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "longDescription" (JD.oneOf [ JD.string, JD.null "" ])


{-| An AddressTypes decoder
-}
addressTypesDecoder : Decoder AddressTypes
addressTypesDecoder =
    JD.list addressTypeDecoder
        |> JD.map (List.map (\({ addressTypeId } as addressType) -> ( addressTypeId, addressType )))
        |> JD.map Dict.fromList


{-| The AddressLike type
-}
type alias AddressLike a =
    { a
        | address1 : String
        , address2 : String
        , country : Maybe ListItem
        , region : Maybe ListItem
        , city : String
        , postalCode : String
    }


{-| The Address type
-}
type alias Address =
    { addressType : Maybe AddressType
    , addressId : Maybe Int
    , name : String
    , address1 : String
    , address2 : String
    , country : Maybe ListItem
    , region : Maybe ListItem
    , city : String
    , postalCode : String
    , phone : PhoneNumber
    , fax : PhoneNumber
    , website : String
    , relativeOrder : Int
    , isPrimaryAddress : Bool
    , includeInInternalDirectory : Bool
    , includeInExternalDirectory : Bool
    }


{-| Get an empty Address
-}
empty : Address
empty =
    { addressType = Nothing
    , addressId = Nothing
    , name = ""
    , address1 = ""
    , address2 = ""
    , country = Nothing
    , region = Nothing
    , city = ""
    , postalCode = ""
    , phone = PhoneNumber.empty
    , fax = PhoneNumber.empty
    , website = ""
    , relativeOrder = 0
    , isPrimaryAddress = False
    , includeInInternalDirectory = False
    , includeInExternalDirectory = False
    }


{-| Get an empty primary Address
-}
emptyPrimaryAddress : Address
emptyPrimaryAddress =
    { empty | isPrimaryAddress = True }


{-| The Address decoder
-}
decoder : Decoder Address
decoder =
    JD.succeed Address
        |> JDP.required "addresType" (JD.map Just addressTypeDecoder)
        |> JDP.required "addressId" (JD.nullable JD.int)
        |> JDP.required "name" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "address1" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "address2" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "country" (JD.maybe ListItem.decoder)
        |> JDP.required "region" (JD.maybe ListItem.decoder)
        |> JDP.required "city" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "postalCode" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "phone" PhoneNumber.decoder
        |> JDP.required "fax" PhoneNumber.decoder
        |> JDP.required "website" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "relativeOrder" JD.int
        |> JDP.required "isPrimaryAddress" JD.bool
        |> JDP.required "includeInInternalDirectory" JD.bool
        |> JDP.required "includeInExternalDirectory" JD.bool


{-| The Countries decoder
-}
countriesDecoder : Decoder Countries
countriesDecoder =
    JD.list countryDataDecoder
        |> JD.map (List.map (\({ countryId } as country) -> ( countryId, country )))
        |> JD.map Dict.fromList


{-| The CountryData decoder
-}
countryDataDecoder : JD.Decoder CountryData
countryDataDecoder =
    JD.succeed CountryData
        |> JDP.required "countryId" JD.int
        |> JDP.required "countryName" JD.string
        |> JDP.required "countryIsoCode" JD.string


{-| The RegionsCountry decoder
-}
regionsCountryDecoder : JD.Decoder RegionsCountry
regionsCountryDecoder =
    let
        countryRegionsDecoder =
            JD.succeed (\countryId regions -> { countryId = countryId, regions = regions })
                |> JDP.required "countryId" JD.int
                |> JDP.required "regions" regionsDecoder
    in
    JD.list countryRegionsDecoder
        |> JD.map (List.map (\{ countryId, regions } -> ( countryId, regions )))
        |> JD.map Dict.fromList


{-| The Regions decoder
-}
regionsDecoder : JD.Decoder Regions
regionsDecoder =
    JD.list regionDataDecoder
        |> JD.map (List.map (\({ regionId } as region) -> ( regionId, region )))
        |> JD.map Dict.fromList


{-| The RegionData decoder
-}
regionDataDecoder : JD.Decoder RegionData
regionDataDecoder =
    JD.succeed RegionData
        |> JDP.required "regionId" JD.int
        |> JDP.required "regionName" JD.string


{-| The Address encoder
-}
encoder : Address -> JE.Value
encoder =
    encoderWith []


{-| The Address encoder with fields
-}
encoderWith : List ( String, JE.Value ) -> Address -> JE.Value
encoderWith fields addressData =
    JE.object
        (fields
            ++ [ ( "addressTypeId", Maybe.map (.addressTypeId >> JE.int) addressData.addressType |> Maybe.withDefault JE.null )
               , ( "addressId", addressData.addressId |> Maybe.map JE.int |> Maybe.withDefault JE.null )
               , ( "name", JE.string addressData.name )
               , ( "address1", JE.string addressData.address1 )
               , ( "address2", JE.string addressData.address2 )
               , ( "city", JE.string addressData.city )
               , ( "regionId", Maybe.map (Tuple.first >> JE.int) addressData.region |> Maybe.withDefault JE.null )
               , ( "countryId", Maybe.map (Tuple.first >> JE.int) addressData.country |> Maybe.withDefault JE.null )
               , ( "postalCode", JE.string addressData.postalCode )
               , ( "phone", PhoneNumber.encoder addressData.phone )
               , ( "fax", PhoneNumber.encoder addressData.fax )
               , ( "website", JE.string addressData.website )
               , ( "isPrimaryAddress", JE.bool addressData.isPrimaryAddress )
               , ( "includeInInternalDirectory", JE.bool addressData.includeInInternalDirectory )
               , ( "includeInExternalDirectory", JE.bool addressData.includeInExternalDirectory )
               ]
        )


{-| Get Regions for a country
-}
getRegionsForCountry : CountryId -> RegionsCountry -> Regions
getRegionsForCountry countryId regions =
    regions
        |> Dict.get countryId
        |> Maybe.withDefault Dict.empty
