module Engage.Form.MembershipTypeList exposing
    ( MembershipType
    , form
    )

{-| Form.MembershipTypeList

@docs MembershipType

@docs form

-}

import Engage.Namespace as Namespace
import Engage.UI.Accordion as Accordion
import Engage.UI.Error as Error
import Html exposing (Html)
import String


{-| The MembershipType type
-}
type alias MembershipType =
    { name : String
    , value : Int
    , description : String
    , price : String
    }


type alias Args a msg =
    { a
        | id : String
        , priceText : String
        , membershipTypeList : List MembershipType
        , labelText : String
        , helpText : String
        , requiredText : Maybe String
        , accordionExpandButtonText : String
        , onChange : { onlyStateChange : Bool } -> Accordion.State -> Maybe MembershipType -> msg
        , status : Error.Status
    }


{-| Get a form view
-}
form : Args a msg -> Accordion.State -> Maybe MembershipType -> Html msg
form ({ id, membershipTypeList, labelText, onChange, status, helpText, accordionExpandButtonText, requiredText } as args) state membershipType =
    let
        onChangeHandler { onlyStateChange } stateValue value =
            onChange { onlyStateChange = onlyStateChange } stateValue (membershipTypeList |> List.filter (.value >> String.fromInt >> (==) value) |> List.head)

        toAccordionItem membershipTypeValue =
            { id = String.fromInt membershipTypeValue.value
            , text = membershipTypeValue.name
            , description =
                membershipTypeValue.description
                    ++ (if String.isEmpty membershipTypeValue.price then
                            ""

                        else
                            "\n\n" ++ args.priceText ++ membershipTypeValue.price
                       )
            }
    in
    Accordion.accordionRadioList
        { namespace = Namespace.engagecore
        , id = id
        , labelText = labelText
        , helpText = helpText
        , requiredText = requiredText
        , onChange = onChangeHandler
        , status = status
        , items = membershipTypeList |> List.map toAccordionItem
        , accordionExpandButtonText = accordionExpandButtonText
        }
        state
        (membershipType |> Maybe.map (.value >> String.fromInt) |> Maybe.withDefault "")
