module Engage.Form.Contact exposing
    ( Attribute, Msg, State, ValidationField(..)
    , completedView, completedViewWithAdditional, contactTypes, countries, countriesToItems, form, initialState, isEmpty, isValid, regions, regionsToItems, required, toAllRegions, update, validateAll, validateAllWith, validateFieldWith, view
    )

{-| Form.Contact

@docs Attribute, Msg, State, ValidationField

@docs completedView, completedViewWithAdditional, contactTypes, countries, countriesToItems, form, initialState, isEmpty, isValid, regions, regionsToItems, required, toAllRegions, update, validateAll, validateAllWith, validateFieldWith, view

-}

import Dict exposing (Dict)
import Engage.CssHelpers
import Engage.Entity.Address as Address exposing (Countries, Regions, RegionsCountry)
import Engage.Entity.Contact as Contact exposing (Contact, ContactType, ContactTypes)
import Engage.Entity.PhoneNumber as PhoneNumber exposing (PhoneNumber)
import Engage.Form.Field as Field
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
import Html.Attributes
import String


{-| The Msg type
-}
type Msg field
    = PrefixUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | FirstNameUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | MiddleNameUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | LastNameUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | SuffixUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | EmailUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | AddressNameUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | Address1Updated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | Address2Updated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | CountryUpdated (ValidationErrors field) Dropdown.State (Maybe ( String, String ))
    | RegionUpdated (ValidationErrors field) Dropdown.State (Maybe ( String, String ))
    | CityUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | PostalCodeUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String
    | ContactTypeUpdated (ValidationErrors field) Dropdown.State (Maybe ContactType)
    | PhoneUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.PhoneState PhoneNumber (Cmd (Msg field))
    | MobilePhoneUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.PhoneState PhoneNumber (Cmd (Msg field))
    | FaxUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.PhoneState PhoneNumber (Cmd (Msg field))
    | IsPrimaryContactUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State Bool
    | IsBillingContactUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State Bool
    | NotesUpdated (ValidationErrors field) { onlyStateChange : Bool } Input.State String


{-| The State type
-}
type State parentField
    = State
        { prefix : Input.State
        , firstName : Input.State
        , middleName : Input.State
        , lastName : Input.State
        , suffix : Input.State
        , email : Input.State
        , addressName : Input.State
        , address1 : Input.State
        , address2 : Input.State
        , country : Dropdown.State
        , region : Dropdown.State
        , city : Input.State
        , postalCode : Input.State
        , validations : ValidationErrors parentField
        , contactType : Dropdown.State
        , phone : Input.PhoneState
        , mobilePhone : Input.PhoneState
        , fax : Input.PhoneState
        , isPrimaryContact : Input.State
        , isBillingContact : Input.State
        , notes : Input.State
        , originalContact : Maybe Contact
        }


{-| Get the initial state
-}
initialState : State parentField
initialState =
    State
        { prefix = Input.initialState
        , firstName = Input.initialState
        , middleName = Input.initialState
        , lastName = Input.initialState
        , suffix = Input.initialState
        , email = Input.initialState
        , addressName = Input.initialState
        , address1 = Input.initialState
        , address2 = Input.initialState
        , country = Dropdown.initialState
        , region = Dropdown.initialState
        , city = Input.initialState
        , postalCode = Input.initialState
        , validations = []
        , contactType = Dropdown.initialState
        , phone = Input.initialPhoneState
        , mobilePhone = Input.initialPhoneState
        , fax = Input.initialPhoneState
        , isPrimaryContact = Input.initialState
        , isBillingContact = Input.initialState
        , notes = Input.initialState
        , originalContact = Nothing
        }


{-| The ValidationField type
-}
type ValidationField
    = Prefix
    | FirstName
    | MiddleName
    | LastName
    | Suffix
    | Email
    | AddressName
    | Address1
    | Address2
    | Country
    | Region
    | PostalCode
    | City
    | ContactType
    | Phone
    | PhoneIsoCode
    | MobilePhone
    | MobilePhoneIsoCode
    | Fax
    | FaxIsoCode
    | Notes
    | IsPrimaryContact
    | IsBillingContact


type alias InternalAttribute =
    { countries : Countries
    , regions : RegionsCountry
    , required : Bool
    , contactTypes : Maybe ContactTypes
    }


emptyAttribute : InternalAttribute
emptyAttribute =
    { countries = Dict.empty
    , regions = Dict.empty
    , required = False
    , contactTypes = Nothing
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


{-| Get the contact types Attribute
-}
contactTypes : ContactTypes -> Attribute
contactTypes value =
    \attribute -> { attribute | contactTypes = Just value }


{-| Get the view
-}
view : Namespace -> Localization -> Countries -> RegionsCountry -> Contact -> Html msg
view namespace localization countries regions data =
    let
        args =
            { namespace = namespace, localization = localization, countries = countries, regions = regions }

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
            [ div [] [ text (data.prefix |> space data.firstName |> space data.middleName |> space data.lastName |> space data.suffix) ]
            , div [] [ text (data.address1 |> space data.address2) ]
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
completedView : Namespace -> Localization -> Contact -> Html msg
completedView namespace localization data =
    completedViewWithAdditional namespace localization [] data


{-| Get the completed view with additional data
-}
completedViewWithAdditional : Namespace -> Localization -> List String -> Contact -> Html msg
completedViewWithAdditional namespace localization additionalText data =
    let
        args =
            { namespace = namespace, localization = localization }

        class =
            args.namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    div [ class [ Css.Sections ] ]
        [ if isEmpty data then
            ul [] [ li [] [ Localization.localizeText "N/A" args ] ]

          else
            ul []
                ([ li [] [ text <| data.address1 ++ " " ++ data.address2 ]
                 , li [] [ text <| data.city ++ " " ++ (Maybe.withDefault "" <| Maybe.map Tuple.second <| data.region) ++ ", " ++ data.postalCode ]
                 , li [] [ text <| Maybe.withDefault "" <| Maybe.map Tuple.second <| data.country ]
                 ]
                    ++ List.map (\txt -> li [] [ text txt ]) additionalText
                )
        ]


{-| Check if the Contact is empty
-}
isEmpty : Contact -> Bool
isEmpty data =
    String.isEmpty data.firstName
        && String.isEmpty data.lastName
        && String.isEmpty data.address1
        && String.isEmpty data.address2
        && String.isEmpty data.city
        && String.isEmpty data.postalCode
        && (data.region == Nothing)
        && (data.country == Nothing)


{-| Get the form view
-}
form : Namespace -> Localization -> (ValidationField -> parentField) -> List Attribute -> State parentField -> Contact -> Html (Msg parentField)
form originalNamespace localization field attributes (State state) contactData =
    let
        attribute =
            Attribute.process emptyAttribute attributes

        class =
            originalNamespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        namespace =
            Namespace.namespace <| Namespace.toString originalNamespace ++ "Contact"

        onContactTypeChangeHandler : ValidationErrors field -> Dropdown.State -> Maybe ( String, String ) -> Msg field
        onContactTypeChangeHandler validations state value =
            value
                |> Maybe.map Tuple.first
                |> Maybe.andThen (String.toInt >> Result.toMaybe)
                |> Maybe.map2 (,) attribute.contactTypes
                |> Maybe.andThen (\( contactTypes, contactTypeId ) -> Dict.get contactTypeId contactTypes)
                |> ContactTypeUpdated validations state
    in
    div
        []
        [ attribute.contactTypes
            |> Maybe.map
                (\contactTypes ->
                    div [ class [ Css.FieldGroup ] ]
                        [ Field.dropdownFieldWithAttributes
                            { namespace = namespace
                            , onChange = onContactTypeChangeHandler
                            , localization = localization
                            , field = field ContactType
                            , required = True
                            , items = contactTypes |> contactTypesToItems
                            }
                            state.validations
                            []
                            state.contactType
                            (Maybe.map (.contactTypeId >> toString) contactData.contactType)
                        ]
                )
            |> Maybe.withDefault HtmlExtra.none
        , div [ class [ Css.FieldGroup ] ]
            [ Field.inputField
                { namespace = namespace
                , onChange = FirstNameUpdated
                , localization = localization
                , field = field FirstName
                , required = True
                }
                state.validations
                state.firstName
                contactData.firstName
            , Field.inputField
                { namespace = namespace
                , onChange = MiddleNameUpdated
                , localization = localization
                , field = field MiddleName
                , required = False
                }
                state.validations
                state.middleName
                contactData.middleName
            , Field.inputField
                { namespace = namespace
                , onChange = LastNameUpdated
                , localization = localization
                , field = field LastName
                , required = True
                }
                state.validations
                state.lastName
                contactData.lastName
            ]
        , div [ class [ Css.FieldGroup ] ]
            [ Field.inputField
                { namespace = namespace
                , onChange = PrefixUpdated
                , localization = localization
                , field = field Prefix
                , required = False
                }
                state.validations
                state.prefix
                contactData.prefix
            , Field.inputField
                { namespace = namespace
                , onChange = SuffixUpdated
                , localization = localization
                , field = field Suffix
                , required = False
                }
                state.validations
                state.suffix
                contactData.suffix
            , Field.inputField
                { namespace = namespace
                , onChange = EmailUpdated
                , localization = localization
                , field = field Email
                , required = True
                }
                state.validations
                state.email
                contactData.email
            ]
        , div [ class [ Css.FieldGroup ] ]
            [ Field.inputFieldWithAttributes
                { namespace = namespace
                , onChange = AddressNameUpdated
                , localization = localization
                , required = False
                , field = field AddressName
                }
                state.validations
                [ Html.Attributes.name "address-name", Html.Attributes.attribute "autocomplete" "address-name" ]
                state.addressName
                contactData.addressName
            ]
        , div [ class [ Css.FieldGroup ] ]
            [ Field.inputFieldWithAttributes
                { namespace = namespace
                , onChange = Address1Updated
                , localization = localization
                , required = attribute.required && True
                , field = field Address1
                }
                state.validations
                [ Html.Attributes.name "address-line1", Html.Attributes.attribute "autocomplete" "address-line1" ]
                state.address1
                contactData.address1
            ]
        , div [ class [ Css.FieldGroup ] ]
            [ Field.inputFieldWithAttributes
                { namespace = namespace
                , onChange = Address2Updated
                , localization = localization
                , field = field Address2
                , required = False
                }
                state.validations
                [ Html.Attributes.name "address-line2", Html.Attributes.attribute "autocomplete" "address-line2" ]
                state.address2
                contactData.address2
            ]
        , div [ class [ Css.FieldGroup ] ]
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
                (Maybe.map (Tuple.first >> toString) contactData.country)
            , Field.dropdownFieldWithAttributes
                { namespace = namespace
                , onChange = RegionUpdated
                , localization = localization
                , field = field Region
                , required = attribute.required
                , items = contactData.country |> Maybe.map Tuple.first |> Maybe.andThen (\country -> Dict.get country attribute.regions) |> Maybe.map regionsToItems |> Maybe.withDefault Dict.empty
                }
                state.validations
                [ Html.Attributes.name "region", Html.Attributes.attribute "autocomplete" "address-level1" ]
                state.region
                (Maybe.map (Tuple.first >> toString) contactData.region)
            ]
        , div [ class [ Css.FieldGroup ] ]
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
                contactData.city
            , Field.inputFieldWithAttributes
                { namespace = namespace
                , onChange = PostalCodeUpdated
                , localization = localization
                , field = field PostalCode
                , required = attribute.required && True
                }
                state.validations
                [ Html.Attributes.name "postal", Html.Attributes.attribute "autocomplete" "postal-code" ]
                state.postalCode
                contactData.postalCode
            ]
        , div [ class [ Css.FieldGroup ] ]
            [ Field.phoneField
                { namespace = namespace
                , onChange = PhoneUpdated
                , localization = localization
                , isoCodeField = field PhoneIsoCode
                , field = field Phone
                , required = True
                }
                state.validations
                state.phone
                contactData.phone
            , Field.phoneField
                { namespace = namespace
                , onChange = MobilePhoneUpdated
                , localization = localization
                , isoCodeField = field MobilePhoneIsoCode
                , field = field MobilePhone
                , required = False
                }
                state.validations
                state.mobilePhone
                contactData.mobilePhone
            , Field.phoneField
                { namespace = namespace
                , onChange = FaxUpdated
                , localization = localization
                , isoCodeField = field FaxIsoCode
                , field = field Fax
                , required = False
                }
                state.validations
                state.fax
                contactData.fax
            ]
        , div [ class [ Css.FieldGroup ] ]
            [ Field.inputField
                { namespace = namespace
                , onChange = NotesUpdated
                , localization = localization
                , field = field Notes
                , required = False
                }
                state.validations
                state.notes
                contactData.notes
            ]
        , div [ class [ Css.FieldGroup ] ]
            [ primaryContactCheckBox namespace localization field (State state) contactData ]
        , div [ class [ Css.FieldGroup ] ]
            [ billingContactCheckBox namespace localization field (State state) contactData ]
        ]


primaryContactCheckBox : Namespace -> Localization -> (ValidationField -> parentField) -> State parentField -> Contact -> Html (Msg parentField)
primaryContactCheckBox originalNamespace localization field (State state) contactData =
    let
        namespace =
            Namespace.namespace <| Namespace.toString originalNamespace ++ "Address"
    in
    case contactData.contactId of
        Nothing ->
            Field.checkbox
                { namespace = namespace
                , onCheck = IsPrimaryContactUpdated
                , localization = localization
                , field = field IsPrimaryContact
                , required = False
                }
                state.validations
                state.isPrimaryContact
                contactData.isPrimaryContact

        Just _ ->
            Field.checkboxWithAttributes
                { namespace = namespace
                , onCheck = IsPrimaryContactUpdated
                , localization = localization
                , field = field IsPrimaryContact
                , required = False
                }
                state.validations
                [ Html.Attributes.disabled (state.originalContact |> Maybe.withDefault contactData |> .isPrimaryContact) ]
                state.isPrimaryContact
                contactData.isPrimaryContact


billingContactCheckBox : Namespace -> Localization -> (ValidationField -> parentField) -> State parentField -> Contact -> Html (Msg parentField)
billingContactCheckBox originalNamespace localization field (State state) contactData =
    let
        namespace =
            Namespace.namespace <| Namespace.toString originalNamespace ++ "Address"
    in
    case contactData.contactId of
        Nothing ->
            Field.checkbox
                { namespace = namespace
                , onCheck = IsBillingContactUpdated
                , localization = localization
                , field = field IsBillingContact
                , required = False
                }
                state.validations
                state.isBillingContact
                contactData.isBillingContact

        Just _ ->
            Field.checkboxWithAttributes
                { namespace = namespace
                , onCheck = IsBillingContactUpdated
                , localization = localization
                , field = field IsBillingContact
                , required = False
                }
                state.validations
                [ Html.Attributes.disabled (state.originalContact |> Maybe.withDefault contactData |> .isBillingContact) ]
                state.isBillingContact
                contactData.isBillingContact


{-| Update the Contact
-}
update : Msg parentField -> State parentField -> Contact -> ( State parentField, Contact, Cmd (Msg parentField) )
update msg (State oldState) data =
    let
        state =
            case oldState.originalContact of
                Just _ ->
                    oldState

                Nothing ->
                    { oldState | originalContact = Just data }
    in
    case msg of
        PrefixUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , prefix = inputState
                }
            , { data | prefix = value }
            , Cmd.none
            )

        FirstNameUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , firstName = inputState
                }
            , { data | firstName = value }
            , Cmd.none
            )

        MiddleNameUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , middleName = inputState
                }
            , { data | middleName = value }
            , Cmd.none
            )

        LastNameUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , lastName = inputState
                }
            , { data | lastName = value }
            , Cmd.none
            )

        SuffixUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , suffix = inputState
                }
            , { data | suffix = value }
            , Cmd.none
            )

        EmailUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , email = inputState
                }
            , { data | email = value }
            , Cmd.none
            )

        AddressNameUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , addressName = inputState
                }
            , { data | addressName = value }
            , Cmd.none
            )

        Address1Updated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , address1 = inputState
                }
            , { data | address1 = value }
            , Cmd.none
            )

        Address2Updated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , address2 = inputState
                }
            , { data | address2 = value }
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
            , { data | city = value }
            , Cmd.none
            )

        PostalCodeUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , postalCode = inputState
                }
            , { data | postalCode = value }
            , Cmd.none
            )

        ContactTypeUpdated validations dropdownState value ->
            ( State
                { state
                    | validations = validations
                    , contactType = dropdownState
                }
            , { data | contactType = value }
            , Cmd.none
            )

        PhoneUpdated validations _ inputState value cmd ->
            ( State
                { state
                    | validations = validations
                    , phone = inputState
                }
            , { data | phone = value }
            , cmd
            )

        MobilePhoneUpdated validations _ inputState value cmd ->
            ( State
                { state
                    | validations = validations
                    , mobilePhone = inputState
                }
            , { data | mobilePhone = value }
            , cmd
            )

        FaxUpdated validations _ inputState value cmd ->
            ( State
                { state
                    | validations = validations
                    , fax = inputState
                }
            , { data | fax = value }
            , cmd
            )

        NotesUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , notes = inputState
                }
            , { data | notes = value }
            , Cmd.none
            )

        IsPrimaryContactUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , isPrimaryContact = inputState
                }
            , { data | isPrimaryContact = value }
            , Cmd.none
            )

        IsBillingContactUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , isBillingContact = inputState
                }
            , { data | isBillingContact = value }
            , Cmd.none
            )


contactTypesToItems : ContactTypes -> Dict String Dropdown.Item
contactTypesToItems countries =
    countries
        |> Dict.values
        |> List.sortBy .shortDescription
        |> List.map (\{ contactTypeId, longDescription } -> ( toString contactTypeId, { value = toString contactTypeId, text = longDescription, enabled = True } ))
        |> Dict.fromList


{-| Convert Countries to an dropdown
-}
countriesToItems : Countries -> Dict String Dropdown.Item
countriesToItems countries =
    countries
        |> Dict.values
        |> List.sortBy .countryName
        |> List.map (\{ countryId, countryName } -> ( toString countryId, { value = toString countryId, text = countryName, enabled = True } ))
        |> Dict.fromList


{-| Convert Regions to an dropdown
-}
regionsToItems : Regions -> Dict String Dropdown.Item
regionsToItems regions =
    regions
        |> Dict.values
        |> List.sortBy .regionName
        |> List.map (\{ regionId, regionName } -> ( toString regionId, { value = toString regionId, text = regionName, enabled = True } ))
        |> Dict.fromList


{-| Validate all of the fields
-}
validateAll : (ValidationField -> parentField) -> State parentField -> Contact -> State parentField
validateAll =
    validateAllWith []


{-| Validate all of the fields with a function
-}
validateAllWith : List (Contact -> ValidationErrors parentField) -> (ValidationField -> parentField) -> State parentField -> Contact -> State parentField
validateAllWith additionalValidations parentField (State state) data =
    State
        { state
            | validations = validateFieldWith additionalValidations parentField data
        }


{-| Validate a field with a function
-}
validateFieldWith : List (Contact -> ValidationErrors parentField) -> (ValidationField -> parentField) -> Contact -> ValidationErrors parentField
validateFieldWith additionalValidations parentField data =
    Validation.validateField
        ([ Validation.validateStringField (Validation.localize (parentField FirstName)) (parentField FirstName) .firstName
         , Validation.validateStringField (Validation.localize (parentField LastName)) (parentField LastName) .lastName
         , Validation.validateStringField (Validation.localize (parentField Email)) (parentField Email) .email
         , Validation.validateStringField (Validation.localize (parentField Address1)) (parentField Address1) .address1
         , Validation.validateMaybeField (Validation.localize (parentField Country)) (parentField Country) .country
         , Validation.validateMaybeField (Validation.localize (parentField Region)) (parentField Region) .region
         , Validation.validateStringField (Validation.localize (parentField PostalCode)) (parentField PostalCode) .postalCode
         , Validation.validateStringField (Validation.localize (parentField City)) (parentField City) .city
         , Validation.validateMaybeField (Validation.localize (parentField ContactType)) (parentField ContactType) .contactType
         , Validation.validateStringField (Validation.localize (parentField Phone)) (parentField Phone) (.phone >> .phoneNumber)
         ]
            ++ additionalValidations
        )
        data


{-| Check if the State is valid
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
