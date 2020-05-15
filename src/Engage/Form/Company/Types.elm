module Engage.Form.Company.Types exposing
    ( CompaniesData
    , CompanyData
    )

import Date exposing (Date)
import Engage.Entity.Address exposing (Address)


type alias CompaniesData data =
    { data
        | currentCompany : CompanyData
        , previousCompany : CompanyData
    }


type alias CompanyData =
    { companyId : Maybe Int
    , participantCompanyId : Maybe Int
    , name : String
    , position : String
    , startDate : Maybe Date
    , endDate : Maybe Date
    , address : Address
    }
