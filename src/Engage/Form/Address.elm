module Engage.Form.Address exposing
    ( Attribute, Msg, State, ValidationField(..)
    , addressTypes, completedView, completedViewWithAdditional, countries, countriesToItems, form, hideFax, hidePrimaryAddressCheckbox, hideWebsite, initialState, isEmpty, isValid, regions, regionsToItems, required, showIncludeInExternalDirectory, showIncludeInInternalDirectory, toAllRegions, update, validateAll, validateAllWith, validateFieldWith, view
    )

{-| Form.Address

@docs Attribute, Msg, State, ValidationField

@docs addressTypes, completedView, completedViewWithAdditional, countries, countriesToItems, form, hideFax, hidePrimaryAddressCheckbox, hideWebsite, initialState, isEmpty, isValid, regions, regionsToItems, required, showIncludeInExternalDirectory, showIncludeInInternalDirectory, toAllRegions, update, validateAll, validateAllWith, validateFieldWith, view

-}

import Dict exposing (Dict)
import Engage.Bool
import Engage.CssHelpers
import Engage.Entity.Address as Address exposing (Address, AddressLike, AddressType, AddressTypes, Countries, Regions, RegionsCountry)
import Engage.Entity.PhoneNumber exposing (PhoneNumber)
import Engage.Form.Field as Field
import Engage.Form.HideOrShow exposing (HideOrShow, Visibility(..))
import Engage.Html.Extra as HtmlExtra
import Engage.ListItem as ListItem exposing (ListItem)
import Engage.Localization as Localization exposing (Localization)
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.String exposing (comma, space)
import Engage.UI.Attribute as Attribute
import Engage.UI.Dropdown as Dropdown
import Engage.UI.Input as Input
import Engage.Validation as Validation exposing (ValidationErrors)
import Html exposing (..)
import Html.Attributes exposing (..)
import String exposing (..)


{-| The Msg type
-}
type Msg field
    = NameUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | AddressUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | UnitUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | CountryUpdated (ValidationErrors field) Dropdown.State (Maybe ( String, String ))
    | RegionUpdated (ValidationErrors field) Dropdown.State (Maybe ( String, String ))
    | CityUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | ZipCodeUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | AddressTypeUpdated (ValidationErrors field) Dropdown.State (Maybe AddressType)
    | PhoneUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.PhoneState PhoneNumber (Cmd (Msg field))
    | FaxUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.PhoneState PhoneNumber (Cmd (Msg field))
    | WebsiteUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | IsPrimaryAddressUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State Bool
    | IncludeInInternalDirectoryUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State Bool
    | IncludeInExternalDirectoryUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State Bool


{-| The State type
-}
type State parentField
    = State
        { name : Input.State
        , address1 : Input.State
        , address2 : Input.State
        , country : Dropdown.State
        , region : Dropdown.State
        , city : Input.State
        , postalCode : Input.State
        , validations : ValidationErrors parentField
        , addressType : Dropdown.State
        , phone : Input.PhoneState
        , fax : Input.PhoneState
        , website : Input.State
        , isPrimaryAddress : Input.State
        , includeInInternalDirectory : Input.State
        , includeInExternalDirectory : Input.State
        , originalAddress : Maybe Address
        }


{-| Get the initial State
-}
initialState : State parentField
initialState =
    State
        { name = Input.initialState
        , address1 = Input.initialState
        , address2 = Input.initialState
        , country = Dropdown.initialState
        , region = Dropdown.initialState
        , city = Input.initialState
        , postalCode = Input.initialState
        , validations = []
        , addressType = Dropdown.initialState
        , phone = Input.initialPhoneState
        , fax = Input.initialPhoneState
        , website = Input.initialState
        , isPrimaryAddress = Input.initialState
        , includeInInternalDirectory = Input.initialState
        , includeInExternalDirectory = Input.initialState
        , originalAddress = Nothing
        }


{-| The ValidationField type
-}
type ValidationField
    = Name
    | Address
    | Unit
    | Country
    | Region
    | ZipCode
    | City
    | AddressType
    | Phone
    | PhoneIsoCode
    | Fax
    | FaxIsoCode
    | Website
    | IsPrimaryAddress
    | IncludeInExternalDirectory
    | IncludeInInternalDirectory


type alias InternalAttribute =
    { countries : Countries
    , regions : RegionsCountry
    , required : Bool
    , addressTypes : Maybe AddressTypes
    , hideFax : Bool
    , hideWebsite : Bool
    , hideAddressPhone : Bool
    , hidePrimaryAddressCheckbox : Bool
    , showIncludeInInternalDirectory : Bool
    , showIncludeInExternalDirectory : Bool
    }


emptyAttribute : InternalAttribute
emptyAttribute =
    { countries = Dict.empty
    , regions = Dict.empty
    , required = False
    , addressTypes = Nothing
    , hideFax = False
    , hideWebsite = False
    , hideAddressPhone = False
    , hidePrimaryAddressCheckbox = False
    , showIncludeInInternalDirectory = False
    , showIncludeInExternalDirectory = False
    }


{-| The Attribute type
-}
type alias Attribute =
    InternalAttribute -> InternalAttribute


{-| Get the countries Attribute
-}
countries : Countries -> Attribute
countries value =
    \attribute -> { attribute | countries = value }


{-| Get the regions Attribute
-}
regions : RegionsCountry -> Attribute
regions value =
    \attribute -> { attribute | regions = value }


{-| Get the required Attribute
-}
required : Bool -> Attribute
required value =
    \attribute -> { attribute | required = value }


{-| Get the address types Attribute
-}
addressTypes : AddressTypes -> Attribute
addressTypes value =
    \attribute -> { attribute | addressTypes = Just value }


{-| Get the hide fax Attribute
-}
hideFax : Attribute
hideFax =
    \attribute -> { attribute | hideFax = True }


{-| Get the hide website Attribute
-}
hideWebsite : Attribute
hideWebsite =
    \attribute -> { attribute | hideWebsite = True }


{-| Get the hide address phone Attribute
-}
hideAddressPhone : Attribute
hideAddressPhone =
    \attribute -> { attribute | hideAddressPhone = True }


{-| Get the hide primary address checkbox Attribute
-}
hidePrimaryAddressCheckbox : Attribute
hidePrimaryAddressCheckbox =
    \attribute -> { attribute | hidePrimaryAddressCheckbox = True }


{-| Get the show include in internal directory Attribute
-}
showIncludeInInternalDirectory : Attribute
showIncludeInInternalDirectory =
    \attribute -> { attribute | showIncludeInInternalDirectory = True }


{-| Get the show include in external directory Attribute
-}
showIncludeInExternalDirectory : Attribute
showIncludeInExternalDirectory =
    \attribute -> { attribute | showIncludeInExternalDirectory = True }


{-| Get the view
-}
view : { a | namespace : Namespace, localization : Localization, countries : Countries, regions : RegionsCountry } -> AddressLike address -> Html msg
view args data =
    let
        class =
            args.namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        maybeCountry =
            data.country
                |> Maybe.andThen (Tuple.first >> (\key -> Dict.get key args.countries))
                |> Maybe.map (.countryName >> Just)
                |> Maybe.withDefault (data.country |> Maybe.map Tuple.second)

        getRegionName : ( Int, String ) -> ( Int, String ) -> Maybe String
        getRegionName ( countryId, _ ) ( regionId, _ ) =
            args.regions
                |> Dict.get countryId
                |> Maybe.andThen (Dict.get regionId)
                |> Maybe.map .regionName

        maybeRegion =
            Maybe.map2 getRegionName data.country data.region
                |> Maybe.withDefault (data.region |> Maybe.map Tuple.second)
    in
    if isEmpty data then
        div [] [ Localization.localizeText "N/A" args ]

    else
        div []
            [ div [] [ text (data.address1 |> space data.address2) ]
            , div []
                [ text
                    (data.city
                        |> comma (maybeRegion |> Maybe.withDefault "")
                        |> comma data.postalCode
                    )
                ]
            , maybeCountry
                |> Maybe.map (\country -> div [] [ text country ])
                |> Maybe.withDefault (text "")
            ]


{-| Get the completed view
-}
completedView : { a | namespace : Namespace, localization : Localization } -> Address -> Html msg
completedView args data =
    completedViewWithAdditional args [] data


{-| Get the completed view with additional
-}
completedViewWithAdditional : { a | namespace : Namespace, localization : Localization } -> List String -> Address -> Html msg
completedViewWithAdditional args additionalText data =
    let
        class =
            args.namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    div [ class [ "Sections" ] ]
        [ if isEmpty data then
            ul [] [ li [] [ Localization.localizeText "N/A" args ] ]

          else
            ul []
                ([ if String.isEmpty data.name then
                    text ""

                   else
                    div [] [ text data.name ]
                 , li [] [ text <| data.address1 ++ " " ++ data.address2 ]
                 , li [] [ text <| data.city ++ " " ++ (Maybe.withDefault "" <| Maybe.map Tuple.second <| data.region) ++ ", " ++ data.postalCode ]
                 , li [] [ text <| Maybe.withDefault "" <| Maybe.map Tuple.second <| data.country ]
                 ]
                    ++ List.map (\txt -> li [] [ text txt ]) additionalText
                )
        ]


{-| Check if the address is empty
-}
isEmpty : AddressLike a -> Bool
isEmpty data =
    String.isEmpty data.address1
        && String.isEmpty data.address2
        && String.isEmpty data.city
        && String.isEmpty data.postalCode
        && (data.region == Nothing)
        && (data.country == Nothing)


{-| Get the form view
-}
form : Namespace -> Localization -> (ValidationField -> parentField) -> List Attribute -> State parentField -> HideOrShow -> Address -> Html (Msg parentField)
form originalNamespace localization field attributes (State state) hideOrShow addressData =
    let
        attributeShow : InternalAttribute
        attributeShow =
            { countries = Dict.empty
            , regions = Dict.empty
            , required = False
            , addressTypes = Nothing
            , hideFax = hideOrShow.fax == Hide
            , hideWebsite = hideOrShow.website == Hide
            , hideAddressPhone = hideOrShow.addressPhone == Hide
            , hidePrimaryAddressCheckbox = False
            , showIncludeInInternalDirectory = False
            , showIncludeInExternalDirectory = False
            }

        attribute =
            Attribute.process attributeShow attributes

        class =
            originalNamespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        namespace =
            Namespace.namespace <| Namespace.toString originalNamespace ++ "Address"

        onAddressTypeChangeHandler : ValidationErrors field -> Dropdown.State -> Maybe ( String, String ) -> Msg field
        onAddressTypeChangeHandler validations state value =
            value
                |> Maybe.map Tuple.first
                |> Maybe.andThen (String.toInt >> Result.toMaybe)
                |> Maybe.map2 (,) attribute.addressTypes
                |> Maybe.andThen (\( addressTypes, addressTypeId ) -> Dict.get addressTypeId addressTypes)
                |> AddressTypeUpdated validations state

        addressTypeDisplayStyle =
            case hideOrShow.addressTypeId of
                Nothing ->
                    [ ( "", "" ) ]

                _ ->
                    [ ( "display", "none" ) ]

        addressNameDisplayStyle =
            if hideOrShow.addressName == Hide then
                [ ( "display", "none" ) ]

            else
                [ ( "", "" ) ]

        regionsForCountry =
            addressData.country
                |> Maybe.map Tuple.first
                |> Maybe.map (\countryId -> Address.getRegionsForCountry countryId attribute.regions)
                |> Maybe.withDefault Dict.empty
    in
    div
        []
        [ attribute.addressTypes
            |> Maybe.map
                (\addressTypes ->
                    div [ class [ "FieldGroup" ], style addressTypeDisplayStyle ]
                        [ Field.dropdownFieldWithAttributes
                            { namespace = namespace
                            , onChange = onAddressTypeChangeHandler
                            , localization = localization
                            , field = field AddressType
                            , required = True
                            , items = addressTypes |> addressTypesToItems
                            }
                            state.validations
                            []
                            state.addressType
                            (Maybe.map (.addressTypeId >> toString) addressData.addressType)
                        ]
                )
            |> Maybe.withDefault HtmlExtra.none
        , div [ class [ "FieldGroup" ], style addressNameDisplayStyle ]
            [ Field.inputFieldWithAttributes
                { namespace = namespace
                , onChange = NameUpdated
                , localization = localization
                , required = False
                , field = field Name
                }
                state.validations
                [ Html.Attributes.name "address-name", Html.Attributes.attribute "autocomplete" "address-name" ]
                state.name
                addressData.name
            ]
        , div [ class [ "FieldGroup" ] ]
            [ Field.inputFieldWithAttributes
                { namespace = namespace
                , onChange = AddressUpdated
                , localization = localization
                , required = attribute.required && True
                , field = field Address
                }
                state.validations
                [ Html.Attributes.name "address-line1", Html.Attributes.attribute "autocomplete" "address-line1" ]
                state.address1
                addressData.address1
            ]
        , div [ class [ "FieldGroup" ] ]
            [ Field.inputFieldWithAttributes
                { namespace = namespace
                , onChange = UnitUpdated
                , localization = localization
                , field = field Unit
                , required = False
                }
                state.validations
                [ Html.Attributes.name "address-line2", Html.Attributes.attribute "autocomplete" "address-line2" ]
                state.address2
                addressData.address2
            ]
        , div [ class [ "FieldGroup" ] ]
            [ Field.dropdownFieldWithAttributes
                { namespace = namespace
                , onChange = CountryUpdated
                , localization = localization
                , field = field Country
                , required = attribute.required
                , items = attribute.countries |> countriesToItems
                }
                state.validations
                [ Html.Attributes.name "country", Html.Attributes.attribute "autocomplete" "country" ]
                state.country
                (Maybe.map (Tuple.first >> toString) addressData.country)
            , Field.dropdownFieldWithAttributes
                { namespace = namespace
                , onChange = RegionUpdated
                , localization = localization
                , field = field Region
                , required = attribute.required
                , items = regionsForCountry |> regionsToItems
                }
                state.validations
                [ Html.Attributes.name "region", Html.Attributes.attribute "autocomplete" "address-level1" ]
                state.region
                (Maybe.map (Tuple.first >> toString) addressData.region)
            ]
        , div [ class [ "FieldGroup" ] ]
            [ Field.inputFieldWithAttributes
                { namespace = namespace
                , onChange = CityUpdated
                , localization = localization
                , field = field City
                , required = attribute.required && True
                }
                state.validations
                [ Html.Attributes.name "city", Html.Attributes.attribute "autocomplete" "address-level2" ]
                state.city
                addressData.city
            , Field.inputFieldWithAttributes
                { namespace = namespace
                , onChange = ZipCodeUpdated
                , localization = localization
                , field = field ZipCode
                , required = attribute.required && True
                }
                state.validations
                [ Html.Attributes.name "postal", Html.Attributes.attribute "autocomplete" "postal-code" ]
                state.postalCode
                addressData.postalCode
            ]
        , div [ class [ "FieldGroup" ] ]
            [ attribute.hideAddressPhone
                |> Engage.Bool.true HtmlExtra.none
                |> Engage.Bool.false
                    (Field.phoneField
                        { namespace = namespace
                        , onChange = PhoneUpdated
                        , localization = localization
                        , isoCodeField = field PhoneIsoCode
                        , field = field Phone
                        , required = False
                        }
                        state.validations
                        state.phone
                        addressData.phone
                    )
            , attribute.hideFax
                |> Engage.Bool.true HtmlExtra.none
                |> Engage.Bool.false
                    (Field.phoneField
                        { namespace = namespace
                        , onChange = FaxUpdated
                        , localization = localization
                        , isoCodeField = field FaxIsoCode
                        , field = field Fax
                        , required = False
                        }
                        state.validations
                        state.fax
                        addressData.fax
                    )
            , attribute.hideWebsite
                |> Engage.Bool.true HtmlExtra.none
                |> Engage.Bool.false
                    (Field.inputField
                        { namespace = namespace
                        , onChange = WebsiteUpdated
                        , localization = localization
                        , field = field Website
                        , required = False
                        }
                        state.validations
                        state.website
                        addressData.website
                    )
            ]
        , attribute.hidePrimaryAddressCheckbox
            |> Engage.Bool.true HtmlExtra.none
            |> Engage.Bool.false
                (div [ class [ "FieldGroup" ] ]
                    [ primaryAddressCheckbox namespace localization field (State state) addressData ]
                )
        , attribute.showIncludeInInternalDirectory
            |> Engage.Bool.true
                (div [ class [ "FieldGroup" ] ]
                    [ Field.checkbox
                        { namespace = namespace
                        , localization = localization
                        , onCheck = IncludeInInternalDirectoryUpdated
                        , field = field IncludeInInternalDirectory
                        , required = False
                        }
                        state.validations
                        state.includeInInternalDirectory
                        addressData.includeInInternalDirectory
                    ]
                )
            |> Engage.Bool.false HtmlExtra.none
        , attribute.showIncludeInExternalDirectory
            |> Engage.Bool.true
                (div [ class [ "FieldGroup" ] ]
                    [ Field.checkbox
                        { namespace = namespace
                        , localization = localization
                        , onCheck = IncludeInExternalDirectoryUpdated
                        , field = field IncludeInExternalDirectory
                        , required = False
                        }
                        state.validations
                        state.includeInExternalDirectory
                        addressData.includeInExternalDirectory
                    ]
                )
            |> Engage.Bool.false HtmlExtra.none
        ]


primaryAddressCheckbox : Namespace -> Localization -> (ValidationField -> parentField) -> State parentField -> Address -> Html (Msg parentField)
primaryAddressCheckbox originalNamespace localization field (State state) addressData =
    let
        namespace =
            Namespace.namespace <| Namespace.toString originalNamespace ++ "Address"
    in
    case addressData.addressId of
        Nothing ->
            Field.checkbox
                { namespace = namespace
                , onCheck = IsPrimaryAddressUpdated
                , localization = localization
                , field = field IsPrimaryAddress
                , required = False
                }
                state.validations
                state.isPrimaryAddress
                addressData.isPrimaryAddress

        Just _ ->
            Field.checkboxWithAttributes
                { namespace = namespace
                , onCheck = IsPrimaryAddressUpdated
                , localization = localization
                , field = field IsPrimaryAddress
                , required = False
                }
                state.validations
                [ Html.Attributes.disabled (state.originalAddress |> Maybe.withDefault addressData |> .isPrimaryAddress) ]
                state.isPrimaryAddress
                addressData.isPrimaryAddress


{-| Update the address
-}
update : Msg parentField -> State parentField -> Address -> ( State parentField, Address, Cmd (Msg parentField) )
update msg (State oldState) data =
    let
        state =
            case oldState.originalAddress of
                Just _ ->
                    oldState

                Nothing ->
                    { oldState | originalAddress = Just data }
    in
    case msg of
        NameUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , name = inputState
                }
            , { data
                | name =
                    if String.length value > 120 then
                        data.name

                    else
                        value
              }
            , Cmd.none
            )

        AddressUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , address1 = inputState
                }
            , { data
                | address1 =
                    if String.length value > 120 then
                        data.address1

                    else
                        value
              }
            , Cmd.none
            )

        UnitUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , address2 = inputState
                }
            , { data
                | address2 =
                    if String.length value > 120 then
                        data.address2

                    else
                        value
              }
            , Cmd.none
            )

        CountryUpdated validations dropdownState value ->
            ( State
                { state
                    | validations = validations
                    , country = dropdownState
                    , region = Dropdown.initialState
                }
            , { data | country = value |> Maybe.andThen ListItem.fromDropdownItem, region = Nothing }
            , Cmd.none
            )

        RegionUpdated validations dropdownState value ->
            ( State
                { state
                    | validations = validations
                    , region = dropdownState
                }
            , { data | region = value |> Maybe.andThen ListItem.fromDropdownItem }
            , Cmd.none
            )

        CityUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , city = inputState
                }
            , { data
                | city =
                    if String.length value > 150 then
                        data.city

                    else
                        value
              }
            , Cmd.none
            )

        ZipCodeUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , postalCode = inputState
                }
            , { data
                | postalCode =
                    if String.length value > 20 then
                        data.postalCode

                    else
                        value
              }
            , Cmd.none
            )

        AddressTypeUpdated validations dropdownState value ->
            ( State
                { state
                    | validations = validations
                    , addressType = dropdownState
                }
            , { data | addressType = value }
            , Cmd.none
            )

        PhoneUpdated validations _ inputState value cmd ->
            ( State
                { state
                    | validations = validations
                    , phone = inputState
                }
            , { data
                | phone =
                    if String.length value.phoneNumber > 30 then
                        data.phone

                    else
                        value
              }
            , cmd
            )

        FaxUpdated validations _ inputState value cmd ->
            ( State
                { state
                    | validations = validations
                    , fax = inputState
                }
            , { data
                | fax =
                    if String.length value.phoneNumber > 30 then
                        data.fax

                    else
                        value
              }
            , cmd
            )

        WebsiteUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , website = inputState
                }
            , { data
                | website =
                    if String.length value > 1000 then
                        data.website

                    else
                        value
              }
            , Cmd.none
            )

        IsPrimaryAddressUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , isPrimaryAddress = inputState
                }
            , { data | isPrimaryAddress = value }
            , Cmd.none
            )

        IncludeInInternalDirectoryUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , includeInInternalDirectory = inputState
                }
            , { data | includeInInternalDirectory = value }
            , Cmd.none
            )

        IncludeInExternalDirectoryUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , includeInExternalDirectory = inputState
                }
            , { data | includeInExternalDirectory = value }
            , Cmd.none
            )


addressTypesToItems : AddressTypes -> Dict String Dropdown.Item
addressTypesToItems countries =
    countries
        |> Dict.values
        |> List.sortBy .shortDescription
        |> List.map (\{ addressTypeId, longDescription } -> ( toString addressTypeId, { value = toString addressTypeId, text = longDescription, enabled = True } ))
        |> Dict.fromList


{-| Convert Countries to items dropdown
-}
countriesToItems : Countries -> Dict String Dropdown.Item
countriesToItems countries =
    countries
        |> Dict.values
        |> List.sortBy .countryName
        |> List.map (\{ countryId, countryName } -> ( toString countryId, { value = toString countryId, text = countryName, enabled = True } ))
        |> Dict.fromList


{-| Convert Regions to items dropdown
-}
regionsToItems : Regions -> Dict String Dropdown.Item
regionsToItems regions =
    regions
        |> Dict.values
        |> List.sortBy .regionName
        |> List.map (\{ regionId, regionName } -> ( toString regionId, { value = toString regionId, text = regionName, enabled = True } ))
        |> Dict.fromList


{-| Validate all fields
-}
validateAll : (ValidationField -> parentField) -> State parentField -> RegionsCountry -> Address -> State parentField
validateAll =
    validateAllWith []


{-| Validate all fields with a function
-}
validateAllWith : List (Address -> ValidationErrors parentField) -> (ValidationField -> parentField) -> State parentField -> RegionsCountry -> Address -> State parentField
validateAllWith additionalValidations parentField (State state) regions data =
    State
        { state
            | validations = validateFieldWith additionalValidations parentField regions data
        }


{-| Validate a field with a function
-}
validateFieldWith : List (Address -> ValidationErrors parentField) -> (ValidationField -> parentField) -> RegionsCountry -> Address -> ValidationErrors parentField
validateFieldWith additionalValidations parentField regions data =
    let
        availableRegions =
            data.country
                |> Maybe.map (\( countryId, _ ) -> Address.getRegionsForCountry countryId regions)
                |> Maybe.withDefault Dict.empty

        regionValidation =
            if Dict.isEmpty availableRegions then
                []

            else
                [ Validation.validateMaybeField (Validation.localize (parentField Region)) (parentField Region) .region ]
    in
    Validation.validateField
        ([ Validation.validateStringField (Validation.localize (parentField Address)) (parentField Address) .address1
         , Validation.validateMaybeField (Validation.localize (parentField Country)) (parentField Country) .country
         , Validation.validateStringField (Validation.localize (parentField City)) (parentField City) .city
         , Validation.validateStringField (Validation.localize (parentField ZipCode)) (parentField ZipCode) .postalCode
         , Validation.validateMaybeField (Validation.localize (parentField AddressType)) (parentField AddressType) .addressType
         ]
            ++ regionValidation
            ++ additionalValidations
        )
        data


{-| Check if the state is valid
-}
isValid : State parentField -> Bool
isValid (State state) =
    Validation.isValid state.validations


{-| Convert RegionsCountry to Regions
-}
toAllRegions : RegionsCountry -> Regions
toAllRegions regionsCountry =
    regionsCountry
        |> Dict.values
        |> List.map Dict.toList
        |> List.concat
        |> Dict.fromList
