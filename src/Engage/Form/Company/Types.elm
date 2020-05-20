module Engage.Form.Company.Types exposing (CompaniesData, CompanyData)

{-| Form.Company.Types

@docs CompaniesData, CompanyData

-}

import Engage.Entity.Address exposing (Address)
import Time exposing (Posix)


{-| A CompaniesData type
-}
type alias CompaniesData data =
    { data
        | currentCompany : CompanyData
        , previousCompany : CompanyData
    }


{-| A CompanyData type
-}
type alias CompanyData =
    { companyId : Maybe Int
    , participantCompanyId : Maybe Int
    , name : String
    , position : String
    , startDate : Maybe Posix
    , endDate : Maybe Posix
    , address : Address
    }
