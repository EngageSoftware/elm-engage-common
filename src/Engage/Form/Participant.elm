module Engage.Form.Participant exposing
    ( AddressRequirement(..)
    , Msg
    , State
    , address
    , completedView
    , countries
    , emptyForm
    , form
    , initialState
    , isValid
    , regions
    , title
    , update
    , validateAll
    , validateWithoutAddress
    )

import Date exposing (Date)
import Dict exposing (Dict)
import Engage.Custom.Form.Css as Css
import Engage.Entity.Account as Account exposing (Account)
import Engage.Entity.Address as Address exposing (Address, Countries, RegionsCountry)
import Engage.Entity.Gender as Gender exposing (Gender)
import Engage.Entity.Participant exposing (Participant)
import Engage.Entity.PhoneNumber as PhoneNumber exposing (PhoneNumber)
import Engage.Form.Address as Address
import Engage.Form.Field as Field
import Engage.Form.Gender as Gender
import Engage.Form.HideOrShow exposing (HideOrShow, Visibility(..))
import Engage.Form.Participant.Css as Css
import Engage.Html.Extra as HtmlExtra
import Engage.ListItem as ListItem exposing (ListItem)
import Engage.Localization as Localization exposing (Localization)
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Ports
import Engage.RemoteData exposing (WebData)
import Engage.String exposing (comma, space)
import Engage.Styles.Class exposing (Size(..))
import Engage.UI.Attribute as Attribute
import Engage.UI.Datepicker as Datepicker
import Engage.UI.Dropdown as Dropdown
import Engage.UI.Error as Error
import Engage.UI.FormControl as FormControl
import Engage.UI.Input as Input
import Engage.UI.Message as Message
import Engage.UI.PictureUpload as PictureUpload
import Engage.Validation as Validation exposing (ValidationErrors)
import Html exposing (..)
import Html.CssHelpers
import IntlPhoneInput.Config
import Json.Encode


{ class } =
    Namespace.engagecore
        |> Namespace.toString
        |> Html.CssHelpers.withNamespace


type Msg
    = FirstNameUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.State String
    | MiddleNameUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.State String
    | LastNameUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.State String
    | EmailUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.State String
    | PhoneUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.PhoneState PhoneNumber (Cmd Msg)
    | MobilePhoneUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.PhoneState PhoneNumber (Cmd Msg)
    | AddressMsg (Address.Msg ValidationField)
    | NoOp
    | BrowseButtonClicked
    | PictureUploadLoaded String
    | PictureSelected (List PictureUpload.File)
    | PictureRemoved
    | GenderUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.State Gender
    | BirthDateUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Datepicker.State (Maybe Date)
    | BirthDateYearUpdated (ValidationErrors ValidationField) Dropdown.State (Maybe ( String, String ))
    | BirthDateMonthUpdated (ValidationErrors ValidationField) Dropdown.State (Maybe ( String, String ))
    | AccountNameUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.State String
    | AccountAddressUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.State String
    | AccountAddress2Updated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.State String
    | AccountCountryUpdated (ValidationErrors ValidationField) Dropdown.State (Maybe ( String, String ))
    | AccountRegionUpdated (ValidationErrors ValidationField) Dropdown.State (Maybe ( String, String ))
    | AccountCityUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.State String
    | AccountPostalCodeUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.State String
    | AccountPhoneUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.State String


type State
    = State
        { firstName : Input.State
        , middleName : Input.State
        , lastName : Input.State
        , email : Input.State
        , phone : Input.PhoneState
        , mobilePhone : Input.PhoneState
        , primaryAddress : Address.State ValidationField
        , gender : Input.State
        , birthDate : Datepicker.State
        , birthDateYear : Dropdown.State
        , birthDateMonth : Dropdown.State
        , accountName : Input.State
        , accountAddress : Input.State
        , accountAddress2 : Input.State
        , accountCountry : Dropdown.State
        , accountRegion : Dropdown.State
        , accountCity : Input.State
        , accountPostalCode : Input.State
        , accountPhone : Input.State
        , validations : ValidationErrors ValidationField
        }


type ValidationField
    = FirstName
    | MiddleName
    | LastName
    | Email
    | Phone
    | PhoneIsoCode
    | MobilePhone
    | MobilePhoneIsoCode
    | Gender
    | ParticipantAddress
    | ParticipantAddressField Address.ValidationField
    | BirthDate
    | BirthDateYear
    | BirthDateMonth
    | AccountName
    | AccountAddress
    | AccountAddress2
    | AccountCountry
    | AccountRegion
    | AccountCity
    | AccountPostalCode
    | AccountPhone


initialState : Date -> State
initialState now =
    State
        { firstName = Input.initialState
        , middleName = Input.initialState
        , lastName = Input.initialState
        , email = Input.initialState
        , phone = Input.initialPhoneState
        , mobilePhone = Input.initialPhoneState
        , primaryAddress = Address.initialState
        , gender = Input.initialState
        , birthDate = Datepicker.initialState now
        , birthDateYear = Dropdown.initialState
        , birthDateMonth = Dropdown.initialState
        , accountName = Input.initialState
        , accountAddress = Input.initialState
        , accountAddress2 = Input.initialState
        , accountCountry = Dropdown.initialState
        , accountRegion = Dropdown.initialState
        , accountCity = Input.initialState
        , accountPostalCode = Input.initialState
        , accountPhone = Input.initialState
        , validations = []
        }


type alias InternalAttribute =
    { title : String
    , countries : Countries
    , regions : RegionsCountry
    , address : Maybe (List Address.Attribute)
    }


emptyAttribute : { a | localization : Localization } -> InternalAttribute
emptyAttribute args =
    { title = Localization.localizeString "Basic Information" args
    , countries = Dict.empty
    , regions = Dict.empty
    , address = Nothing
    }


type alias Attribute =
    InternalAttribute -> InternalAttribute


title : String -> Attribute
title value =
    \attribute -> { attribute | title = value }


countries : Countries -> Attribute
countries value =
    \attribute -> { attribute | countries = value }


regions : RegionsCountry -> Attribute
regions value =
    \attribute -> { attribute | regions = value }


address : List Address.Attribute -> Attribute
address attributes =
    \attribute -> { attribute | address = Just attributes }


form : Namespace -> Localization -> Date -> List Attribute -> State -> Participant -> HideOrShow -> List (Html.Html Msg)
form namespace localization now attributes (State state) participantData hideOrShow =
    let
        args =
            { namespace = namespace, localization = localization }

        attribute : InternalAttribute
        attribute =
            Attribute.process (emptyAttribute args) attributes

        yearsDropDown =
            let
                createDropdownItem val =
                    { value = toString val
                    , text = toString val
                    , enabled = True
                    }
            in
            hideOrShow.birthDateYears
                |> List.map (\year -> ( toString year, createDropdownItem year ))
                |> Dict.fromList

        monthsDropDown =
            let
                createDropdownItem val month =
                    { value = toString (val + 1)
                    , text = month
                    , enabled = True
                    }
            in
            hideOrShow.birthDateMonths
                |> List.filter (\month -> not (String.isEmpty month))
                |> List.indexedMap (\index month -> createDropdownItem index month)
                |> List.map (\item -> ( item.value, item ))
                |> Dict.fromList

        birthDateForm : Html Msg
        birthDateForm =
            let
                birthDateYear =
                    div [ class [ Css.FieldGroup ] ]
                        [ Field.dropdownFieldValueSort
                            { namespace = namespace
                            , onChange = BirthDateYearUpdated
                            , localization = localization
                            , field = BirthDateYear
                            , required = hideOrShow.birthDateRequired
                            , items = yearsDropDown
                            }
                            state.validations
                            state.birthDateYear
                            (Maybe.map (Tuple.first >> toString) participantData.birthDateYear)
                            True
                        ]
            in
            case hideOrShow.birthDate of
                "MM/dd/yyyy" ->
                    div [ class [ Css.FieldGroup ] ]
                        [ Field.datepickerField
                            { namespace = namespace
                            , onChange = \validation -> BirthDateUpdated validation { onlyStateChange = False }
                            , onStateChange = \validation state -> BirthDateUpdated validation { onlyStateChange = True } state participantData.birthDate
                            , localization = localization
                            , field = BirthDate
                            , required = hideOrShow.birthDateRequired
                            , now = now
                            }
                            state.validations
                            state.birthDate
                            participantData.birthDate
                        ]

                "MM/yyyy" ->
                    birthDateYear

                "yyyy" ->
                    birthDateYear

                _ ->
                    HtmlExtra.none

        birthDateMonth : Html Msg
        birthDateMonth =
            case hideOrShow.birthDate of
                "MM/yyyy" ->
                    div [ class [ Css.FieldGroup ] ]
                        [ Field.dropdownFieldValueSort
                            { namespace = namespace
                            , onChange = BirthDateMonthUpdated
                            , localization = localization
                            , field = BirthDateMonth
                            , required = hideOrShow.birthDateRequired
                            , items = monthsDropDown
                            }
                            state.validations
                            state.birthDateMonth
                            (Maybe.map (Tuple.first >> toString) participantData.birthDateMonth)
                            False
                        ]

                _ ->
                    HtmlExtra.none

        genderForm : Html Msg
        genderForm =
            if hideOrShow.gender == Hide then
                HtmlExtra.none

            else
                Gender.form
                    [ Gender.field Gender
                    , Gender.onChange GenderUpdated
                    , Gender.localization localization
                    ]
                    state.validations
                    state.gender
                    participantData.gender

        headshotForm : Html Msg
        headshotForm =
            if hideOrShow.headshot == Hide then
                div [] []

            else
                div [ class [ Css.ParticipantPicture ] ]
                    [ PictureUpload.pictureUpload
                        args.namespace
                        (Namespace.toString args.namespace ++ "ParticipantProfileUpload")
                        [ PictureUpload.browse (Localization.localizeString "Browse" args) BrowseButtonClicked
                        , PictureUpload.onLoad PictureUploadLoaded
                        , PictureUpload.onFiles PictureSelected
                        , PictureUpload.picture participantData.profilePicture
                        , PictureUpload.remove (Localization.localizeString "Remove Picture" args) PictureRemoved
                        , PictureUpload.dropZone (Localization.localizeString "Drop your picture here" args)
                        ]
                    ]

        readOnlyControl : ValidationField -> String -> Html Msg
        readOnlyControl field value =
            FormControl.formControl
                { namespace = namespace
                , size = Large
                , id = Field.fieldId namespace field
                , labelText = Field.localizeLabel { field = field, localization = localization }
                , helpText = Field.localizeHelp { field = field, localization = localization }
                , requiredText = Nothing
                , onValidationStateChange = always NoOp
                , status = Error.Unknown
                }
                Message.initialState
                (text value)

        accountForm : Account -> Html Msg
        accountForm account =
            let
                regionsForCountry =
                    account.country
                        |> Maybe.map Tuple.first
                        |> Maybe.map (\countryId -> Address.getRegionsForCountry countryId attribute.regions)
                        |> Maybe.withDefault Dict.empty
            in
            case hideOrShow.account of
                Hide ->
                    HtmlExtra.none

                Show ->
                    div [ class [ toString Css.Form, toString Css.ParticipantAccount ] ]
                        [ div [ class [ Css.ParticipantForm ] ]
                            [ h3 [ class [ toString Css.FormTitle, toString Css.ParticipantTitle ] ]
                                [ Localization.localizeTextWithDefault
                                    "Account Information"
                                    "Account Information.Label"
                                    args
                                ]
                            , div [ class [ Css.FieldGroup ] ]
                                [ Field.inputField
                                    { namespace = namespace
                                    , onChange = AccountNameUpdated
                                    , localization = localization
                                    , field = AccountName
                                    , required = True
                                    }
                                    state.validations
                                    state.accountName
                                    account.name
                                ]
                            , div [ class [ Css.FieldGroup ] ]
                                [ Field.inputField
                                    { namespace = namespace
                                    , onChange = AccountAddressUpdated
                                    , localization = localization
                                    , field = AccountAddress
                                    , required = True
                                    }
                                    state.validations
                                    state.accountAddress
                                    account.address
                                ]
                            , div [ class [ Css.FieldGroup ] ]
                                [ Field.inputField
                                    { namespace = namespace
                                    , onChange = AccountAddress2Updated
                                    , localization = localization
                                    , field = AccountAddress2
                                    , required = False
                                    }
                                    state.validations
                                    state.accountAddress2
                                    account.address2
                                ]
                            , div [ class [ Css.FieldGroup ] ]
                                [ Field.dropdownField
                                    { namespace = namespace
                                    , onChange = AccountCountryUpdated
                                    , localization = localization
                                    , field = AccountCountry
                                    , required = True
                                    , items = attribute.countries |> Address.countriesToItems
                                    }
                                    state.validations
                                    state.accountCountry
                                    (Maybe.map (Tuple.first >> toString) account.country)
                                , Field.dropdownField
                                    { namespace = namespace
                                    , onChange = AccountRegionUpdated
                                    , localization = localization
                                    , field = AccountRegion
                                    , required = True
                                    , items = regionsForCountry |> Address.regionsToItems
                                    }
                                    state.validations
                                    state.accountRegion
                                    (Maybe.map (Tuple.first >> toString) account.region)
                                ]
                            , div [ class [ Css.FieldGroup ] ]
                                [ Field.inputField
                                    { namespace = namespace
                                    , onChange = AccountCityUpdated
                                    , localization = localization
                                    , field = AccountCity
                                    , required = True
                                    }
                                    state.validations
                                    state.accountCity
                                    account.city
                                , Field.inputField
                                    { namespace = namespace
                                    , onChange = AccountPostalCodeUpdated
                                    , localization = localization
                                    , field = AccountPostalCode
                                    , required = True
                                    }
                                    state.validations
                                    state.accountPostalCode
                                    account.postalCode
                                ]
                            , div [ class [ Css.FieldGroup ] ]
                                [ Field.inputField
                                    { namespace = namespace
                                    , onChange = AccountPhoneUpdated
                                    , localization = localization
                                    , field = AccountPhone
                                    , required = False
                                    }
                                    state.validations
                                    state.accountPhone
                                    account.phone
                                ]
                            ]
                        ]

                ReadOnly ->
                    div [ class [ toString Css.Form, toString Css.ParticipantAccount ] ]
                        [ div [ class [ Css.ParticipantForm ] ]
                            [ h3 [ class [ toString Css.FormTitle, toString Css.ParticipantTitle ] ]
                                [ Localization.localizeTextWithDefault
                                    "Account Information"
                                    "Account Information.Label"
                                    args
                                ]
                            , div [ class [ Css.FieldGroup ] ]
                                [ readOnlyControl AccountName account.name
                                , readOnlyControl AccountAddress account.address
                                , readOnlyControl AccountAddress2 account.address2
                                , readOnlyControl AccountCountry (account.country |> Maybe.map Tuple.second |> Maybe.withDefault "")
                                , readOnlyControl AccountRegion (account.region |> Maybe.map Tuple.second |> Maybe.withDefault "")
                                , readOnlyControl AccountCity account.city
                                , readOnlyControl AccountPostalCode account.postalCode
                                , readOnlyControl AccountPhone account.phone
                                ]
                            ]
                        ]

        participantForm : List (Html Msg)
        participantForm =
            [ div [ class [ Css.FieldGroup ] ]
                [ Field.inputField
                    { namespace = args.namespace
                    , onChange = FirstNameUpdated
                    , localization = args.localization
                    , field = FirstName
                    , required = True
                    }
                    state.validations
                    state.firstName
                    participantData.firstName
                , Field.inputField
                    { namespace = namespace
                    , onChange = MiddleNameUpdated
                    , localization = localization
                    , field = MiddleName
                    , required = False
                    }
                    state.validations
                    state.middleName
                    participantData.middleName
                , Field.inputField
                    { namespace = args.namespace
                    , onChange = LastNameUpdated
                    , localization = args.localization
                    , field = LastName
                    , required = True
                    }
                    state.validations
                    state.lastName
                    participantData.lastName
                ]
            , div [ class [ Css.FieldGroup ] ]
                [ Field.inputField
                    { namespace = namespace
                    , onChange = EmailUpdated
                    , localization = localization
                    , field = Email
                    , required = True
                    }
                    state.validations
                    state.email
                    participantData.email
                , Field.phoneField
                    { namespace = namespace
                    , onChange = PhoneUpdated
                    , localization = localization
                    , isoCodeField = PhoneIsoCode
                    , field = Phone
                    , required = True
                    }
                    state.validations
                    state.phone
                    participantData.phone
                , Field.phoneField
                    { namespace = namespace
                    , onChange = MobilePhoneUpdated
                    , localization = localization
                    , field = MobilePhone
                    , isoCodeField = MobilePhoneIsoCode
                    , required = False
                    }
                    state.validations
                    state.mobilePhone
                    participantData.mobilePhone
                , birthDateMonth
                , birthDateForm
                , genderForm
                ]
            ]

        addressFormData : Address
        addressFormData =
            participantData.primaryAddress
                |> Maybe.withDefault (Address.empty |> (\empty -> { empty | isPrimaryAddress = True }))

        addressForm : List (Html Msg)
        addressForm =
            attribute.address
                |> Maybe.map (\addressAttributes -> addressAttributes ++ [ Address.required True, Address.hidePrimaryAddressCheckbox, Address.countries attribute.countries, Address.regions attribute.regions ])
                |> Maybe.map (\addressAttributes -> Address.form namespace localization ParticipantAddressField addressAttributes state.primaryAddress hideOrShow addressFormData)
                |> Maybe.map (Html.map AddressMsg)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        formTitle : String
        formTitle =
            if String.isEmpty attribute.title then
                Localization.localizeStringWithDefault
                    "Basic Information"
                    "WizardTitle.Label"
                    args

            else
                attribute.title
    in
    [ accountForm (participantData.account |> Maybe.withDefault (Account.empty |> (\account -> { account | country = addressFormData.country })))
    , div [ class [ toString Css.Form, toString Css.Participant ] ]
        [ h3 [ class [ toString Css.FormTitle, toString Css.ParticipantTitle ] ] [ text formTitle ]
        , headshotForm
        , div [ class [ Css.ParticipantForm ] ]
            (participantForm ++ addressForm)
        ]
    ]


update : Msg -> State -> Participant -> ( State, Participant, Cmd Msg )
update msg (State state) data =
    case msg of
        NoOp ->
            ( State state, data, Cmd.none )

        FirstNameUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , firstName = inputState
                }
            , { data
                | firstName =
                    if String.length value > 50 then
                        data.firstName

                    else
                        value
              }
            , Cmd.none
            )

        MiddleNameUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , middleName = inputState
                }
            , { data
                | middleName =
                    if String.length value > 50 then
                        data.middleName

                    else
                        value
              }
            , Cmd.none
            )

        LastNameUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , lastName = inputState
                }
            , { data
                | lastName =
                    if String.length value > 50 then
                        data.lastName

                    else
                        value
              }
            , Cmd.none
            )

        EmailUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , email = inputState
                }
            , { data
                | email =
                    if String.length value > 512 then
                        data.email

                    else
                        value
              }
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

        MobilePhoneUpdated validations _ inputState value cmd ->
            ( State
                { state
                    | validations = validations
                    , mobilePhone = inputState
                }
            , { data
                | mobilePhone =
                    if String.length value.phoneNumber > 30 then
                        data.mobilePhone

                    else
                        value
              }
            , cmd
            )

        AddressMsg addressMsg ->
            updateAddress addressMsg (State state) data (data.primaryAddress |> Maybe.withDefault Address.empty)

        BrowseButtonClicked ->
            ( State state, data, Cmd.none )

        PictureUploadLoaded domId ->
            ( State state
            , data
            , Engage.Ports.send PictureUpload.PictureUploadLoaded (Json.Encode.string domId)
            )

        PictureSelected files ->
            ( State state
            , files
                |> List.head
                |> Maybe.map (\file -> { data | profilePicture = file.dataURL })
                |> Maybe.withDefault data
            , Cmd.none
            )

        PictureRemoved ->
            ( State state, { data | profilePicture = "" }, Cmd.none )

        GenderUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , gender = inputState
                }
            , { data | gender = value }
            , Cmd.none
            )

        BirthDateUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , birthDate = inputState
                }
            , { data | birthDate = value }
            , Cmd.none
            )

        BirthDateYearUpdated validations dropdownState value ->
            ( State
                { state
                    | validations = validations
                    , birthDateYear = dropdownState
                }
            , { data | birthDateYear = value |> Maybe.andThen ListItem.fromDropdownItem }
            , Cmd.none
            )

        BirthDateMonthUpdated validations dropdownState value ->
            ( State
                { state
                    | validations = validations
                    , birthDateMonth = dropdownState
                }
            , { data | birthDateMonth = value |> Maybe.andThen ListItem.fromDropdownItem }
            , Cmd.none
            )

        AccountNameUpdated validations _ inputState value ->
            let
                account =
                    data.account |> Maybe.withDefault Account.empty

                updatedAccount =
                    { account | name = value }
            in
            ( State
                { state
                    | validations = validations
                    , accountName = inputState
                }
            , { data | account = Just updatedAccount }
            , Cmd.none
            )

        AccountAddressUpdated validations _ inputState value ->
            let
                account =
                    data.account |> Maybe.withDefault Account.empty

                updatedAccount =
                    { account | address = value }
            in
            ( State
                { state
                    | validations = validations
                    , accountAddress = inputState
                }
            , { data | account = Just updatedAccount }
            , Cmd.none
            )

        AccountAddress2Updated validations _ inputState value ->
            let
                account =
                    data.account |> Maybe.withDefault Account.empty

                updatedAccount =
                    { account | address2 = value }
            in
            ( State
                { state
                    | validations = validations
                    , accountAddress2 = inputState
                }
            , { data | account = Just updatedAccount }
            , Cmd.none
            )

        AccountCountryUpdated validations dropdownState value ->
            let
                account =
                    data.account |> Maybe.withDefault Account.empty

                updatedAccount =
                    { account | country = value |> Maybe.andThen ListItem.fromDropdownItem, region = Nothing }
            in
            ( State
                { state
                    | validations = validations
                    , accountCountry = dropdownState
                    , accountRegion = Dropdown.initialState
                }
            , { data | account = Just updatedAccount }
            , Cmd.none
            )

        AccountRegionUpdated validations dropdownState value ->
            let
                account =
                    data.account |> Maybe.withDefault Account.empty

                updatedAccount =
                    { account | region = value |> Maybe.andThen ListItem.fromDropdownItem }
            in
            ( State
                { state
                    | validations = validations
                    , accountRegion = dropdownState
                }
            , { data | account = Just updatedAccount }
            , Cmd.none
            )

        AccountCityUpdated validations _ inputState value ->
            let
                account =
                    data.account |> Maybe.withDefault Account.empty

                updatedAccount =
                    { account | city = value }
            in
            ( State
                { state
                    | validations = validations
                    , accountCity = inputState
                }
            , { data | account = Just updatedAccount }
            , Cmd.none
            )

        AccountPostalCodeUpdated validations _ inputState value ->
            let
                account =
                    data.account |> Maybe.withDefault Account.empty

                updatedAccount =
                    { account | postalCode = value }
            in
            ( State
                { state
                    | validations = validations
                    , accountPostalCode = inputState
                }
            , { data | account = Just updatedAccount }
            , Cmd.none
            )

        AccountPhoneUpdated validations _ inputState value ->
            let
                account =
                    data.account |> Maybe.withDefault Account.empty

                updatedAccount =
                    { account | phone = value }
            in
            ( State
                { state
                    | validations = validations
                    , accountPhone = inputState
                }
            , { data | account = Just updatedAccount }
            , Cmd.none
            )


updateAddress : Address.Msg ValidationField -> State -> Participant -> Address -> ( State, Participant, Cmd Msg )
updateAddress addressMsg (State state) participant address =
    let
        ( addressState, addressData, cmd ) =
            Address.update addressMsg state.primaryAddress address
    in
    ( State { state | primaryAddress = addressState }
    , { participant | primaryAddress = Just addressData }
    , cmd |> Cmd.map AddressMsg
    )


type AddressRequirement
    = AddressRequired
    | AddressOptional


validate : AddressRequirement -> Bool -> RegionsCountry -> HideOrShow -> State -> Participant -> State
validate addressRequirement validatePrimaryAddress regions hideOrShow (State state) data =
    let
        addressValidation =
            case addressRequirement of
                AddressRequired ->
                    [ Validation.validateMaybeField (Validation.localize ParticipantAddress) ParticipantAddress .primaryAddress ]

                AddressOptional ->
                    []

        accountValidations =
            case hideOrShow.account of
                Show ->
                    let
                        availableRegions =
                            data.account
                                |> Maybe.andThen .country
                                |> Maybe.map (\( countryId, _ ) -> Address.getRegionsForCountry countryId regions)
                                |> Maybe.withDefault Dict.empty

                        regionValidation =
                            if Dict.isEmpty availableRegions then
                                []

                            else
                                [ Validation.validateMaybeField (Validation.localize AccountRegion) AccountRegion (\p -> Maybe.andThen .region p.account) ]
                    in
                    [ Validation.validateMaybeStringField (Validation.localize AccountName) AccountName (\p -> Maybe.map .name p.account)
                    , Validation.validateMaybeStringField (Validation.localize AccountAddress) AccountAddress (\p -> Maybe.map .address p.account)
                    , Validation.validateMaybeField (Validation.localize AccountCountry) AccountCountry (\p -> Maybe.andThen .country p.account)
                    , Validation.validateMaybeStringField (Validation.localize AccountCity) AccountCity (\p -> Maybe.map .city p.account)
                    , Validation.validateMaybeStringField (Validation.localize AccountPostalCode) AccountPostalCode (\p -> Maybe.map .postalCode p.account)
                    ]
                        ++ regionValidation

                _ ->
                    []

        primaryAddress =
            if validatePrimaryAddress then
                data.primaryAddress
                    |> Maybe.map (Address.validateAll ParticipantAddressField state.primaryAddress regions)
                    |> Maybe.withDefault state.primaryAddress

            else
                state.primaryAddress

        validations =
            Validation.validateField
                ([ Validation.validateStringField (Validation.localize FirstName) FirstName .firstName
                 , Validation.validateStringField (Validation.localize LastName) LastName .lastName
                 , Validation.validateStringField (Validation.localize Email) Email .email
                 , Validation.validateStringField (Validation.localize Phone) Phone (.phone >> .phoneNumber)
                 , Validation.validateStringField (Validation.localize PhoneIsoCode) PhoneIsoCode (.phone >> .isoCode)
                 ]
                    ++ addressValidation
                    ++ accountValidations
                )
                data
    in
    State { state | validations = validations, primaryAddress = primaryAddress }


validateAll : AddressRequirement -> RegionsCountry -> HideOrShow -> State -> Participant -> State
validateAll addressRequirement regions hideOrShow state data =
    validate addressRequirement True regions hideOrShow state data


validateWithoutAddress : AddressRequirement -> RegionsCountry -> HideOrShow -> State -> Participant -> State
validateWithoutAddress addressRequirement regions hideOrShow state data =
    validate addressRequirement False regions hideOrShow state data


isValid : State -> Bool
isValid (State state) =
    Validation.isValid state.validations && Address.isValid state.primaryAddress


completedView : Participant -> Html msg
completedView data =
    let
        intlPhoneInputConfig =
            IntlPhoneInput.Config.defaultConfig (\_ _ _ -> NoOp)
    in
    div [ class [ Css.Sections ] ]
        [ ul []
            [ li [] [ text <| fullName data ]
            , li [] [ text data.email ]
            , li [] [ text <| PhoneNumber.format intlPhoneInputConfig data.phone ]
            , li [] [ text <| PhoneNumber.format intlPhoneInputConfig data.mobilePhone ]
            , data.primaryAddress
                |> Maybe.map
                    (\address ->
                        if String.isEmpty address.name then
                            HtmlExtra.none

                        else
                            li [] [ text address.name ]
                    )
                |> Maybe.withDefault HtmlExtra.none
            , data.primaryAddress |> Maybe.map (\address -> li [] [ text (address.address1 |> space address.address2) ]) |> Maybe.withDefault HtmlExtra.none
            , data.primaryAddress |> Maybe.map (\address -> li [] [ text (address.city |> space (Maybe.withDefault "" <| Maybe.map Tuple.second <| address.region) |> comma address.postalCode) ]) |> Maybe.withDefault HtmlExtra.none
            , data.primaryAddress |> Maybe.map (\address -> li [] [ text <| Maybe.withDefault "" <| Maybe.map Tuple.second <| address.country ]) |> Maybe.withDefault HtmlExtra.none
            , data.account |> Maybe.map (\account -> li [] [ text account.name ]) |> Maybe.withDefault HtmlExtra.none
            , data.account |> Maybe.map (\account -> li [] [ text (account.address |> space account.address2) ]) |> Maybe.withDefault HtmlExtra.none
            , data.account |> Maybe.map (\account -> li [] [ text (account.city |> space (Maybe.withDefault "" <| Maybe.map Tuple.second <| account.region) |> comma account.postalCode) ]) |> Maybe.withDefault HtmlExtra.none
            , data.account |> Maybe.map (\account -> li [] [ text <| Maybe.withDefault "" <| Maybe.map Tuple.second <| account.country ]) |> Maybe.withDefault HtmlExtra.none
            ]
        ]


fullName : Participant -> String
fullName data =
    data.firstName |> space data.middleName |> space data.lastName


emptyForm :
    Int
    -> Date
    -> String
    ->
        { name : String
        , id : Int
        , stepResponse : WebData a
        , state : State
        }
emptyForm id now name =
    { name = name
    , id = id
    , state = initialState now
    , stepResponse = Engage.RemoteData.NotAsked
    }
