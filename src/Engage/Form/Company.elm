module Engage.Form.Company exposing
    ( Msg, State
    , cast, completedView, emptyForm, form, initialState, isValid, update, validateAll
    )

{-| Form.Company

@docs Msg, State

@docs cast, completedView, emptyForm, form, initialState, isValid, update, validateAll

-}

import Date exposing (Date)
import Engage.CssHelpers
import Engage.Custom.Form.Css as Css
import Engage.Entity.Address as Address exposing (RegionsCountry)
import Engage.Form.Address as Address
import Engage.Form.Company.Json exposing (emptyCompanies)
import Engage.Form.Company.Types exposing (CompaniesData, CompanyData)
import Engage.Form.Field as Field
import Engage.Form.HideOrShow exposing (HideOrShow, showAll)
import Engage.Localization as Localization exposing (Localization)
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.RemoteData exposing (WebData)
import Engage.UI.Datepicker as Datepicker
import Engage.UI.Input as Input
import Engage.Validation as Validation exposing (ValidationErrors)
import Html exposing (..)
import String


class =
    Namespace.engagecore
        |> Namespace.toString
        |> Engage.CssHelpers.withNamespace


type alias CompanyState =
    { name : Input.State
    , position : Input.State
    , startDate : Datepicker.State
    , endDate : Datepicker.State
    , address : Address.State ValidationField
    , validations : ValidationErrors ValidationField
    }


{-| The State type
-}
type State
    = State StateData


type alias StateData =
    { currentCompany : CompanyState
    , previousCompany : CompanyState
    }


{-| Get the initial state
-}
initialState : Date -> State
initialState now =
    State
        { currentCompany =
            { name = Input.initialState
            , position = Input.initialState
            , startDate = Datepicker.initialState now
            , endDate = Datepicker.initialState now
            , address = Address.initialState
            , validations = []
            }
        , previousCompany =
            { name = Input.initialState
            , position = Input.initialState
            , startDate = Datepicker.initialState now
            , endDate = Datepicker.initialState now
            , address = Address.initialState
            , validations = []
            }
        }


type ValidationField
    = CurrentCompanyField CompanyValidationField
    | PreviousCompanyField CompanyValidationField
    | CurrentCompanyAddress Address.ValidationField
    | PreviousCompanyAddress Address.ValidationField


type CompanyValidationField
    = CompanyName
    | Position
    | StartDate
    | EndDate


{-| The Msg type
-}
type Msg
    = CurrentCompany CompanyMsg
    | PreviousCompany CompanyMsg


type CompanyMsg
    = CompanyNameUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.State String
    | PositionUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.State String
    | StartDateStateUpdated (ValidationErrors ValidationField) Datepicker.State
    | EndDateStateUpdated (ValidationErrors ValidationField) Datepicker.State
    | StartDateUpdated (ValidationErrors ValidationField) Datepicker.State (Maybe Date)
    | EndDateUpdated (ValidationErrors ValidationField) Datepicker.State (Maybe Date)
    | AddressMsg (Address.Msg ValidationField)


type alias FormArgs a =
    { a
        | namespace : Namespace
        , localization : Localization
        , title : String
        , countries : Address.Countries
        , regions : Address.RegionsCountry
        , now : Date
    }


{-| Get the form view
-}
form : FormArgs a -> State -> CompaniesData data -> Html.Html Msg
form args (State state) data =
    div [ class [ Css.Form ] ]
        [ Html.map CurrentCompany <| currentCompany args state data
        , Html.map PreviousCompany <| previousCompany args state data
        ]


currentCompany : FormArgs a -> StateData -> CompaniesData data -> Html.Html CompanyMsg
currentCompany args state data =
    let
        namespace =
            Namespace.namespace <| Namespace.toString args.namespace ++ "Company"
    in
    fieldset [ class [ Css.FormSection ] ]
        [ legend [] [ Localization.localizeText "CurrentCompany.Title" args ]
        , div [ class [ Css.FieldGroup ] ]
            [ Field.inputField
                { namespace = namespace
                , onChange = CompanyNameUpdated
                , localization = args.localization
                , field = CurrentCompanyField CompanyName
                , required = True
                }
                state.currentCompany.validations
                state.currentCompany.name
                data.currentCompany.name
            , Field.inputField
                { namespace = namespace
                , onChange = PositionUpdated
                , localization = args.localization
                , field = CurrentCompanyField Position
                , required = True
                }
                state.currentCompany.validations
                state.currentCompany.position
                data.currentCompany.position
            ]
        , Html.map AddressMsg <|
            Address.form
                args.namespace
                args.localization
                CurrentCompanyAddress
                [ Address.countries args.countries
                , Address.regions args.regions
                , Address.required True
                ]
                state.currentCompany.address
                showAll
                data.currentCompany.address
        , div [ class [ Css.FieldGroup ] ]
            [ Field.dateField
                { namespace = args.namespace
                , onChange = StartDateUpdated
                , onStateChange = StartDateStateUpdated
                , localization = args.localization
                , field = CurrentCompanyField StartDate
                , required = True
                , now = args.now
                }
                state.currentCompany.validations
                state.currentCompany.startDate
                data.currentCompany.startDate
            , div [] []
            ]
        ]


previousCompany : FormArgs a -> StateData -> CompaniesData data -> Html.Html CompanyMsg
previousCompany args state data =
    let
        namespace =
            Namespace.namespace <| Namespace.toString args.namespace ++ "Company"
    in
    fieldset [ class [ Css.FormSection ] ]
        [ legend [] [ Localization.localizeText "PreviousCompany.Title" args ]
        , div [ class [ Css.FieldGroup ] ]
            [ Field.inputField
                { namespace = namespace
                , onChange = CompanyNameUpdated
                , localization = args.localization
                , field = PreviousCompanyField CompanyName
                , required = True
                }
                state.previousCompany.validations
                state.previousCompany.name
                data.previousCompany.name
            , Field.inputField
                { namespace = namespace
                , onChange = PositionUpdated
                , localization = args.localization
                , field = PreviousCompanyField Position
                , required = True
                }
                state.previousCompany.validations
                state.previousCompany.position
                data.previousCompany.position
            ]
        , Html.map AddressMsg <|
            Address.form
                args.namespace
                args.localization
                PreviousCompanyAddress
                [ Address.countries args.countries
                , Address.regions args.regions
                , Address.required <| not <| String.isEmpty data.previousCompany.name
                ]
                state.previousCompany.address
                showAll
                data.previousCompany.address
        , div [ class [ Css.FieldGroup ] ]
            [ Field.dateField
                { namespace = args.namespace
                , onChange = StartDateUpdated
                , onStateChange = StartDateStateUpdated
                , localization = args.localization
                , field = PreviousCompanyField StartDate
                , required = True
                , now = args.now
                }
                state.previousCompany.validations
                state.previousCompany.startDate
                data.previousCompany.startDate
            , Field.dateField
                { namespace = args.namespace
                , onChange = EndDateUpdated
                , onStateChange = EndDateStateUpdated
                , localization = args.localization
                , field = PreviousCompanyField EndDate
                , required = True
                , now = args.now
                }
                state.previousCompany.validations
                state.previousCompany.endDate
                data.previousCompany.endDate
            ]
        ]


{-| Get a completed view
-}
completedView : { a | localization : Localization } -> CompaniesData data -> Html.Html msg
completedView args data =
    div [ class [ Css.Sections ] ]
        [ ul []
            (li [] [ strong [] [ Localization.localizeText "CurrentCompany.Title" args ] ]
                :: (if String.isEmpty data.currentCompany.name then
                        [ li [] [ Localization.localizeText "NA" args ]
                        ]

                    else
                        [ li [] [ text data.currentCompany.name ]
                        , li [] [ text data.currentCompany.position ]
                        , li [] [ text <| data.currentCompany.address.address1 ++ " " ++ data.currentCompany.address.address2 ]
                        , li [] [ text <| data.currentCompany.address.city ++ " " ++ (Maybe.withDefault "" <| Maybe.map Tuple.second <| data.currentCompany.address.region) ++ ", " ++ data.currentCompany.address.postalCode ]
                        , li [] [ text <| Maybe.withDefault "" <| Maybe.map Tuple.second <| data.currentCompany.address.country ]
                        ]
                   )
            )
        , ul []
            (li [] [ strong [] [ Localization.localizeText "PreviousCompany.Title" args ] ]
                :: (if String.isEmpty data.previousCompany.name then
                        [ li [] [ Localization.localizeText "NA" args ]
                        ]

                    else
                        [ li [] [ text data.previousCompany.name ]
                        , li [] [ text data.previousCompany.position ]
                        , li [] [ text <| data.previousCompany.address.address1 ++ " " ++ data.previousCompany.address.address2 ]
                        , li [] [ text <| data.previousCompany.address.city ++ " " ++ (Maybe.withDefault "" <| Maybe.map Tuple.second <| data.previousCompany.address.region) ++ ", " ++ data.previousCompany.address.postalCode ]
                        , li [] [ text <| Maybe.withDefault "" <| Maybe.map Tuple.second <| data.previousCompany.address.country ]
                        ]
                   )
            )
        ]


{-| Update a CompaniesData
-}
update : Msg -> State -> CompaniesData data -> ( State, CompaniesData data, Cmd Msg )
update msg (State state) data =
    case msg of
        CurrentCompany companyMsg ->
            let
                ( companyState, updatedCompany, cmd ) =
                    updateCompany companyMsg state.currentCompany data.currentCompany
            in
            ( State { state | currentCompany = companyState }
            , { data | currentCompany = updatedCompany }
            , cmd |> Cmd.map CurrentCompany
            )

        PreviousCompany companyMsg ->
            let
                ( companyState, updatedCompany, cmd ) =
                    updateCompany companyMsg state.previousCompany data.previousCompany
            in
            ( State { state | previousCompany = companyState }
            , { data | previousCompany = updatedCompany }
            , cmd |> Cmd.map PreviousCompany
            )


updateCompany : CompanyMsg -> CompanyState -> CompanyData -> ( CompanyState, CompanyData, Cmd CompanyMsg )
updateCompany msg companyState companyData =
    case msg of
        CompanyNameUpdated validations _ inputState value ->
            ( { companyState | name = inputState, validations = validations }
            , { companyData | name = value }
            , Cmd.none
            )

        PositionUpdated validations _ inputState value ->
            ( { companyState | position = inputState, validations = validations }
            , { companyData | position = value }
            , Cmd.none
            )

        StartDateStateUpdated validations datepickerState ->
            ( { companyState | startDate = datepickerState, validations = validations }
            , companyData
            , Cmd.none
            )

        StartDateUpdated validations datepickerState value ->
            ( { companyState | startDate = datepickerState, validations = validations }
            , { companyData | startDate = value }
            , Cmd.none
            )

        EndDateStateUpdated validations datepickerState ->
            ( { companyState | endDate = datepickerState, validations = validations }
            , companyData
            , Cmd.none
            )

        EndDateUpdated validations datepickerState value ->
            ( { companyState | endDate = datepickerState, validations = validations }
            , { companyData | endDate = value }
            , Cmd.none
            )

        AddressMsg addressMsg ->
            updateAddress addressMsg companyState companyData


updateAddress : Address.Msg ValidationField -> CompanyState -> CompanyData -> ( CompanyState, CompanyData, Cmd CompanyMsg )
updateAddress addressMsg companyState companyData =
    let
        ( addressState, addressData, cmd ) =
            Address.update addressMsg companyState.address companyData.address
    in
    ( { companyState | address = addressState }
    , { companyData | address = addressData }
    , Cmd.map AddressMsg cmd
    )


{-| Validate all fields
-}
validateAll : State -> RegionsCountry -> CompaniesData data -> State
validateAll (State state) regions data =
    let
        currentCompany =
            state.currentCompany

        previousCompany =
            state.previousCompany
    in
    State
        { state
            | currentCompany =
                { currentCompany
                    | address = Address.validateAll CurrentCompanyAddress state.currentCompany.address regions data.currentCompany.address
                    , validations =
                        Validation.validateField
                            [ Validation.validateStringField (Validation.localize (CurrentCompanyField CompanyName)) (CurrentCompanyField CompanyName) (.currentCompany >> .name)
                            , Validation.validateMaybeField (Validation.localize (CurrentCompanyField StartDate)) (CurrentCompanyField StartDate) (.currentCompany >> .startDate)
                            ]
                            data
                }
            , previousCompany =
                { previousCompany
                    | address =
                        if String.isEmpty data.previousCompany.name then
                            Address.initialState

                        else
                            Address.validateAll PreviousCompanyAddress state.previousCompany.address regions data.previousCompany.address
                    , validations =
                        Validation.validateField
                            []
                            data
                }
        }


{-| Check if the State is valid
-}
isValid : State -> Bool
isValid (State state) =
    Validation.isValid state.currentCompany.validations
        && Validation.isValid state.previousCompany.validations
        && Address.isValid state.currentCompany.address
        && Address.isValid state.previousCompany.address


{-| Cast a CompaniesData
-}
cast : CompaniesData other -> CompaniesData {}
cast companies =
    { currentCompany = companies.currentCompany
    , previousCompany = companies.previousCompany
    }


{-| Get an empty form
-}
emptyForm :
    Int
    -> Date
    -> String
    ->
        { id : Int
        , name : String
        , stepResponse : WebData a
        , companies : CompaniesData {}
        , state : State
        }
emptyForm id now name =
    let
        correctName =
            if String.isEmpty name then
                "Qualifying Experience"

            else
                name
    in
    { id = id
    , name = correctName
    , companies = emptyCompanies
    , state = initialState now
    , stepResponse = Engage.RemoteData.NotAsked
    }
