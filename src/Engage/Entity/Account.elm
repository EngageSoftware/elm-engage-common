module Engage.Entity.Account exposing
    ( Account
    , decoder
    , empty
    , encoder
    , encoderWith
    )

import Engage.ListItem as ListItem exposing (ListItem)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE


type alias Account =
    { accountId : Maybe Int
    , name : String
    , description : String
    , address : String
    , address2 : String
    , city : String
    , region : Maybe ListItem
    , country : Maybe ListItem
    , postalCode : String
    , phone : String
    }


empty : Account
empty =
    { accountId = Nothing
    , name = ""
    , description = ""
    , address = ""
    , address2 = ""
    , city = ""
    , region = Nothing
    , country = Nothing
    , postalCode = ""
    , phone = ""
    }


decoder : Decoder Account
decoder =
    JDP.decode Account
        |> JDP.required "accountId" (JD.map Just JD.int)
        |> JDP.required "name" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "description" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "address" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "address2" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "city" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "region" (JD.maybe ListItem.decoder)
        |> JDP.required "country" (JD.maybe ListItem.decoder)
        |> JDP.required "postalCode" (JD.oneOf [ JD.null "", JD.string ])
        |> JDP.required "phone" (JD.oneOf [ JD.null "", JD.string ])


encoder : Account -> JE.Value
encoder =
    encoderWith []


encoderWith : List ( String, JE.Value ) -> Account -> JE.Value
encoderWith fields account =
    JE.object
        (fields
            ++ [ ( "accountId", Maybe.map JE.int account.accountId |> Maybe.withDefault JE.null )
               , ( "name", JE.string account.name )
               , ( "description", JE.string account.description )
               , ( "phone", JE.string account.phone )
               , ( "address", JE.string account.address )
               , ( "address2", JE.string account.address2 )
               , ( "city", JE.string account.city )
               , ( "regionId", Maybe.map (Tuple.first >> JE.int) account.region |> Maybe.withDefault JE.null )
               , ( "countryId", Maybe.map (Tuple.first >> JE.int) account.country |> Maybe.withDefault JE.null )
               , ( "zipCode", JE.string account.postalCode )
               , ( "isCurrent", JE.bool True )
               ]
        )
