module Engage.Form.Company.Json exposing (decoder, emptyCompanies, emptyCompany, encoder, encoderWith)

{-| Form.Company.Json

@docs decoder, emptyCompanies, emptyCompany, encoder, encoderWith

-}

import Engage.Entity.Address as Address
import Engage.Form.Company.Types exposing (CompaniesData, CompanyData)
import Iso8601
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import String


{-| Get the encoder
-}
encoder : { a | participantId : Int, companies : CompaniesData {} } -> Encode.Value
encoder =
    encoderWith []


{-| Get the encoder with data
-}
encoderWith : List ( String, Encode.Value ) -> { a | participantId : Int, companies : CompaniesData {} } -> Encode.Value
encoderWith additional { participantId, companies } =
    Encode.object
        (additional
            ++ [ ( "participantId", Encode.int participantId )
               , ( "companies"
                 , Encode.list companyEncoder
                    [ ( True, companies.currentCompany )
                    , ( False, companies.previousCompany )
                    ]
                 )
               ]
        )


{-| Get the company encoder
-}
companyEncoder : ( Bool, CompanyData ) -> Encode.Value
companyEncoder ( isCurrent, company ) =
    if String.isEmpty company.name then
        Encode.null

    else
        Encode.object
            [ ( "companyId", Maybe.map Encode.int company.companyId |> Maybe.withDefault Encode.null )
            , ( "participantCompanyId", Maybe.map Encode.int company.participantCompanyId |> Maybe.withDefault Encode.null )
            , ( "name", Encode.string company.name )
            , ( "position", Encode.string company.position )
            , ( "startDate"
              , company.startDate
                    |> Maybe.map Iso8601.encode
                    |> Maybe.withDefault Encode.null
              )
            , ( "endDate"
              , company.endDate
                    |> Maybe.map Iso8601.encode
                    |> Maybe.withDefault Encode.null
              )
            , ( "address1", Encode.string company.address.address1 )
            , ( "address2", Encode.string company.address.address2 )
            , ( "city", Encode.string company.address.city )
            , ( "regionId"
              , company.address.region
                    |> Maybe.map (Tuple.first >> Encode.int)
                    |> Maybe.withDefault Encode.null
              )
            , ( "countryId"
              , company.address.country
                    |> Maybe.map (Tuple.first >> Encode.int)
                    |> Maybe.withDefault Encode.null
              )
            , ( "postalCode", Encode.string company.address.postalCode )
            , ( "isCurrent", Encode.bool isCurrent )
            ]


{-| The CompaniesData decoder
-}
decoder : Decoder (CompaniesData {})
decoder =
    let
        listToCompanies companies =
            case companies of
                current :: previous :: _ ->
                    Decode.succeed { currentCompany = current, previousCompany = previous }

                current :: [] ->
                    Decode.succeed { currentCompany = current, previousCompany = emptyCompany }

                _ ->
                    Decode.fail <| "Need at least 2 companies"
    in
    Decode.field "companies" (Decode.list companyDecoder)
        |> Decode.andThen listToCompanies


companyDecoder : Decoder CompanyData
companyDecoder =
    succeed CompanyData
        |> required "companyId" (Decode.maybe Decode.int)
        |> required "participantCompanyId" (Decode.maybe Decode.int)
        |> required "name" Decode.string
        |> required "position" (Decode.maybe Decode.string |> Decode.map (Maybe.withDefault ""))
        |> required "startDate" (Decode.maybe Iso8601.decoder)
        |> required "endDate" (Decode.maybe Iso8601.decoder)
        |> required "address" (Decode.maybe Address.decoder |> Decode.map (Maybe.withDefault Address.empty))


{-| Get an empty CompaniesData
-}
emptyCompanies : CompaniesData {}
emptyCompanies =
    { currentCompany = emptyCompany
    , previousCompany = emptyCompany
    }


{-| Get an empty CompanyData
-}
emptyCompany : CompanyData
emptyCompany =
    { companyId = Nothing
    , participantCompanyId = Nothing
    , name = ""
    , position = ""
    , startDate = Nothing
    , endDate = Nothing
    , address = Address.empty
    }
