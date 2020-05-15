module Engage.Entity.Address exposing
    ( Address
    , AddressLike
    , AddressType
    , AddressTypes
    , Countries
    , CountryId
    , RegionId
    , Regions
    , RegionsCountry
    , addressTypeDecoder
    , addressTypesDecoder
    , countriesDecoder
    , decoder
    , empty
    , emptyAddressType
    , emptyPrimaryAddress
    , encoder
    , encoderWith
    , getRegionsForCountry
    , regionsCountryDecoder
    )

import Dict exposing (Dict)
import Engage.Entity.PhoneNumber as PhoneNumber exposing (PhoneNumber)
import Engage.ListItem as ListItem exposing (ListItem)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE


type alias CountryId =
    Int


type alias RegionId =
    Int


type alias AddressTypeId =
    Int


type alias Countries =
    Dict CountryId CountryData


type alias CountryData =
    { countryId : Int, countryName : String, countryIsoCode : String }


type alias RegionsCountry =
    Dict CountryId Regions


type alias Regions =
    Dict RegionId RegionData


type alias AddressTypes =
    Dict AddressTypeId AddressType


type alias RegionData =
    { regionId : Int, regionName : String }


type alias AddressType =
    { addressTypeId : Int
    , shortDescription : String
    , longDescription : String
    }


emptyAddressType : AddressType
emptyAddressType =
    { addressTypeId = 0
    , shortDescription = ""
    , longDescription = ""
    }


addressTypeDecoder : Decoder AddressType
addressTypeDecoder =
    JDP.decode AddressType
        |> JDP.required "addressTypeId" JD.int
        |> JDP.required "shortDescription" (JD.oneOf [ JD.string, JD.null "" ])
        |> JDP.required "longDescription" (JD.oneOf [ JD.string, JD.null "" ])


addressTypesDecoder : Decoder AddressTypes
addressTypesDecoder =
    JD.list addressTypeDecoder
        |> JD.map (List.map (\({ addressTypeId } as addressType) -> ( addressTypeId, addressType )))
        |> JD.map Dict.fromList


type alias AddressLike a =
    { a
        | address1 : String
        , address2 : String
        , country : Maybe ListItem
        , region : Maybe ListItem
        , city : String
        , postalCode : String
    }


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


emptyPrimaryAddress : Address
emptyPrimaryAddress =
    { empty | isPrimaryAddress = True }


decoder : Decoder Address
decoder =
    JDP.decode Address
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


countriesDecoder : Decoder Countries
countriesDecoder =
    JD.list countryDataDecoder
        |> JD.map (List.map (\({ countryId } as country) -> ( countryId, country )))
        |> JD.map Dict.fromList


countryDataDecoder : JD.Decoder CountryData
countryDataDecoder =
    JDP.decode CountryData
        |> JDP.required "countryId" JD.int
        |> JDP.required "countryName" JD.string
        |> JDP.required "countryIsoCode" JD.string


regionsCountryDecoder : JD.Decoder RegionsCountry
regionsCountryDecoder =
    let
        countryRegionsDecoder =
            JDP.decode (\countryId regions -> { countryId = countryId, regions = regions })
                |> JDP.required "countryId" JD.int
                |> JDP.required "regions" regionsDecoder
    in
    JD.list countryRegionsDecoder
        |> JD.map (List.map (\{ countryId, regions } -> ( countryId, regions )))
        |> JD.map Dict.fromList


regionsDecoder : JD.Decoder Regions
regionsDecoder =
    JD.list regionDataDecoder
        |> JD.map (List.map (\({ regionId } as region) -> ( regionId, region )))
        |> JD.map Dict.fromList


regionDataDecoder : JD.Decoder RegionData
regionDataDecoder =
    JDP.decode RegionData
        |> JDP.required "regionId" JD.int
        |> JDP.required "regionName" JD.string


encoder : Address -> JE.Value
encoder =
    encoderWith []


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


getRegionsForCountry : CountryId -> RegionsCountry -> Regions
getRegionsForCountry countryId regions =
    regions
        |> Dict.get countryId
        |> Maybe.withDefault Dict.empty
