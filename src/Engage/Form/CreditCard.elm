module Engage.Form.CreditCard exposing
    ( BillingAddress
    , FormArgs
    , Msg
    , OutMsg(..)
    , State
    , billingAddressSaved
    , empty
    , form
    , initialState
    , isEditingBillingAddress
    , isLoading
    , isValid
    , ready
    , toggleBillingAddress
    , update
    , updateCardError
    , validateAll
    , validateName
    )

import Engage.CreditCard.Ports
import Engage.Custom.Form.Css as CustomFormCss
import Engage.Entity.Address as Address exposing (RegionsCountry)
import Engage.Entity.PhoneNumber as PhoneNumber exposing (PhoneNumber)
import Engage.Form.Address as Address exposing (ValidationField(..))
import Engage.Form.CreditCard.Css as Css
import Engage.Form.Field as Field
import Engage.Form.HideOrShow exposing (HideOrShow, showAll)
import Engage.Html.Extra as HtmlExtra
import Engage.Http as Http
import Engage.Localization as Localization exposing (Localization)
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Ports
import Engage.RemoteData
import Engage.Styles.Class exposing (Size(..))
import Engage.UI.Button as Button
import Engage.UI.Error as Error
import Engage.UI.FormControl as FormControl
import Engage.UI.Input as Input
import Engage.UI.Input.Css as InputCss
import Engage.UI.Loading as Loading
import Engage.UI.Loading.Css as LoadingCss
import Engage.UI.Message as Message
import Engage.UI.MessageType as MessageType
import Engage.UI.Wizard.Css as WizardCss
import Engage.Validation as Validation exposing (ValidationErrors)
import Html exposing (..)
import Html.Attributes
import Html.CssHelpers
import Html.Events exposing (onClick)
import Http
import IntlPhoneInput.Config
import Json.Encode
import RemoteData
import String


{ class, id } =
    Html.CssHelpers.withNamespace <| Namespace.toString Namespace.engagecore


stripeCardId : String
stripeCardId =
    Namespace.toString Namespace.engagecore ++ "-stripe-card"


type alias BillingAddress =
    { fullName : String
    , address : Address.Address
    }


empty : BillingAddress
empty =
    { fullName = ""
    , address = Address.empty
    }


type Msg
    = NoOp
    | FullNameUpdated (ValidationErrors ValidationField) { onlyStateChange : Bool } Input.State String
    | CreditCardUpdated Message.State
    | AddressMsg (Address.Msg ValidationField)
    | EditBillingAddress
    | CancelEditBillingAddress
    | SaveBillingAddress
    | StripeCardReady
    | BillingAddressStateChanged Message.State


type OutMsg
    = NoOut
    | UpdateBillingAddress Address.Address


initialState : State
initialState =
    State
        { fullName = Input.initialState
        , billingAddress = View
        , validations = []
        , status = Engage.RemoteData.NotAsked
        , creditCard = Message.initialState
        , creditCardError = Error.Unknown
        , creditCardStatus = RemoteData.Loading
        , billingAddressState = Message.initialState
        }


type BillingAddressState
    = View
    | Edit (Address.State ValidationField) Address.Address


type State
    = State StateData


type alias StateData =
    { fullName : Input.State
    , validations : ValidationErrors ValidationField
    , billingAddress : BillingAddressState
    , status : Engage.RemoteData.WebData ()
    , creditCard : Message.State
    , creditCardError : Error.Status
    , creditCardStatus : RemoteData.RemoteData String ()
    , billingAddressState : Message.State
    }


type ValidationField
    = FullName
    | PhoneNumber
    | PhoneNumberIsoCode
    | BillingAddressField Address.ValidationField


type alias FormArgs a =
    { a
        | namespace : Namespace
        , localization : Localization
        , countries : Address.Countries
        , regions : Address.RegionsCountry
    }



-- VIEWS


isLoading : State -> Bool
isLoading (State state) =
    RemoteData.isLoading state.creditCardStatus


ready : State -> State
ready (State state) =
    State { state | creditCardStatus = RemoteData.Success () }


form : FormArgs a -> Bool -> State -> BillingAddress -> Html Msg
form args showCreditCard (State state) data =
    div [ class [ CustomFormCss.Form ], class [ Css.CreditCard ] ]
        [ div [ class [ CustomFormCss.FormSection ], class [ Css.CreditCardSection ] ]
            [ h3 [ class [ Css.FormTitle ] ] [ Localization.localizeText "PaymentInfo" args ]
            , div [ class [ Css.CreditCardForm ] ]
                [ div [ class [ CustomFormCss.FieldGroup ] ]
                    [ Field.inputFieldWithAttributes
                        { namespace = args.namespace
                        , onChange = FullNameUpdated
                        , localization = args.localization
                        , field = FullName
                        , required = True
                        }
                        state.validations
                        [ Html.Attributes.name "ccname", Html.Attributes.attribute "autocomplete" "cc-name" ]
                        state.fullName
                        data.fullName
                    ]
                , if showCreditCard then
                    div [ class [ CustomFormCss.FieldGroup ] ]
                        [ FormControl.formControl
                            { namespace = args.namespace
                            , size = Large
                            , id = stripeCardId
                            , labelText = Localization.localizeString "CreditCard" args
                            , helpText = ""
                            , requiredText = Just (Localization.localizeStringWithDefault "Required" "Required" args)
                            , status = state.creditCardError
                            , onValidationStateChange = CreditCardUpdated
                            }
                            state.creditCard
                            (div
                                [ id [ stripeCardId ], class [ InputCss.Input Large ] ]
                                [ HtmlExtra.domLoadNotifier StripeCardReady ]
                            )
                        ]

                  else
                    HtmlExtra.none
                ]
            ]
        , billingAddressView args state data
        ]


billingAddressView : FormArgs a -> StateData -> BillingAddress -> Html Msg
billingAddressView args state data =
    let
        intlPhoneInputConfig =
            IntlPhoneInput.Config.defaultConfig (\_ _ _ -> NoOp)

        billingAddressValidationStatus =
            state.validations
                |> List.filter isBillingAddressValidation
                |> Validation.toError
    in
    div [ class [ CustomFormCss.FormSection ], class [ Css.BillingAddressSection ] ]
        [ h3 [ class [ Css.FormTitle ] ] [ Localization.localizeText "BillingAddress" args ]
        , div [ class [ Css.CompletedBillingAddress <| Css.toVisibility <| not <| isEditingBillingAddress (State state) ] ]
            [ div [ class [ CustomFormCss.FormCompleted ] ]
                [ div
                    [ class [ CustomFormCss.FormCompletedContent ] ]
                    [ Address.completedViewWithAdditional args
                        [ PhoneNumber.format intlPhoneInputConfig data.address.phone ]
                        data.address
                    , Error.inlineError
                        { namespace = args.namespace
                        , status = Error.localize (\str -> Localization.localizeString str args) billingAddressValidationStatus
                        , onChange = BillingAddressStateChanged
                        }
                        state.billingAddressState
                    ]
                , Button.divertSmall
                    { namespace = args.namespace
                    , attributes = [ onClick EditBillingAddress ]
                    , text = Localization.localizeString "Edit Billing Address" args
                    }
                ]
            ]
        , case state.billingAddress of
            View ->
                div [ class [ Css.BillingAddressForm <| Css.toVisibility <| isEditingBillingAddress (State state) ] ] []

            Edit addressState editingAddress ->
                div [ class [ Css.BillingAddressForm <| Css.toVisibility <| isEditingBillingAddress (State state) ] ]
                    [ Html.map AddressMsg <|
                        Address.form
                            args.namespace
                            args.localization
                            BillingAddressField
                            [ Address.countries args.countries
                            , Address.regions args.regions
                            , Address.required True
                            , Address.hideFax
                            , Address.hideWebsite
                            , Address.hidePrimaryAddressCheckbox
                            ]
                            addressState
                            showAll
                            editingAddress
                    , state.status
                        |> Engage.RemoteData.toError
                        |> Maybe.map (billingAddressErrorView args)
                        |> Maybe.withDefault HtmlExtra.none
                    , div [ class [ WizardCss.NavigationControl ] ]
                        [ Button.divert
                            { namespace = args.namespace
                            , attributes = [ onClick CancelEditBillingAddress ]
                            , text = Localization.localizeString "Cancel" args
                            }
                        , Button.primary
                            { namespace = args.namespace
                            , attributes = [ onClick SaveBillingAddress ]
                            , text = Localization.localizeString "Save Billing Address" args
                            }
                        ]
                    , if Engage.RemoteData.isLoading state.status then
                        Loading.loadingOverlay
                            { namespace = Namespace.engagecore }
                            LoadingCss.Ring
                            (Localization.localizeStringWithDefault "Saving..." "Saving" args)
                            []

                      else
                        HtmlExtra.none
                    ]
        ]


billingAddressErrorView : FormArgs a -> Http.Error -> Html Msg
billingAddressErrorView args error =
    Message.message
        { namespace = Namespace.engagecore
        , messageType = MessageType.Error
        }
        [ Localization.localizeStringWithDefault
            (Http.getErrorMessage args error)
            "InitializationFailure.Error"
            args
            |> Html.text
        ]


toggleBillingAddress : BillingAddress -> State -> State
toggleBillingAddress data (State state) =
    State
        { state
            | billingAddress =
                if Address.isEmpty data.address then
                    Edit Address.initialState Address.empty

                else
                    View
        }


billingAddressSaved : Engage.RemoteData.WebData () -> State -> State
billingAddressSaved webData (State state) =
    case webData of
        Engage.RemoteData.NotAsked ->
            State { state | status = webData }

        Engage.RemoteData.Loading ->
            State { state | status = webData }

        Engage.RemoteData.Reloading _ ->
            State { state | status = webData }

        Engage.RemoteData.Failure _ ->
            State { state | status = webData }

        Engage.RemoteData.FailureWithData _ _ ->
            State { state | status = webData }

        Engage.RemoteData.Success _ ->
            State { state | billingAddress = View, status = webData }


return : OutMsg -> ( State, BillingAddress, Cmd Msg ) -> ( State, BillingAddress, Cmd Msg, OutMsg )
return outMsg ( state, data, cmd ) =
    ( state, data, cmd, outMsg )


noCmd : ( State, BillingAddress ) -> ( State, BillingAddress, Cmd msg )
noCmd ( state, data ) =
    ( state, data, Cmd.none )


update : RegionsCountry -> Msg -> State -> BillingAddress -> ( State, BillingAddress, Cmd Msg, OutMsg )
update regions msg (State state) data =
    case msg of
        NoOp ->
            ( State state, data, Cmd.none ) |> return NoOut

        FullNameUpdated validations _ inputState value ->
            ( State
                { state
                    | validations = validations
                    , fullName = inputState
                }
            , { data | fullName = value }
            )
                |> noCmd
                |> return NoOut

        AddressMsg addressMsg ->
            updateAddress addressMsg (State state) data
                |> return NoOut

        EditBillingAddress ->
            ( State
                { state
                    | billingAddress = Edit Address.initialState data.address
                    , status = Engage.RemoteData.NotAsked
                }
            , data
            )
                |> noCmd
                |> return NoOut

        CancelEditBillingAddress ->
            ( State { state | billingAddress = View }, data )
                |> noCmd
                |> return NoOut

        SaveBillingAddress ->
            saveBillingAddress regions state data

        StripeCardReady ->
            ( State state
            , data
            , Engage.Ports.send Engage.CreditCard.Ports.DomReady (Json.Encode.string stripeCardId)
            )
                |> return NoOut

        CreditCardUpdated message ->
            ( State { state | creditCard = message }
            , data
            )
                |> noCmd
                |> return NoOut

        BillingAddressStateChanged messageState ->
            ( State { state | billingAddressState = messageState }
            , data
            )
                |> noCmd
                |> return NoOut


saveBillingAddress : RegionsCountry -> StateData -> BillingAddress -> ( State, BillingAddress, Cmd Msg, OutMsg )
saveBillingAddress regions state data =
    case state.billingAddress of
        Edit addressState addressData ->
            let
                additionalValidations =
                    [ Validation.validateStringField (Validation.localize PhoneNumber) (BillingAddressField Phone) (.phone >> .phoneNumber)
                    , Validation.validateStringField (Validation.localize PhoneNumberIsoCode) (BillingAddressField PhoneIsoCode) (.phone >> .isoCode)
                    ]

                isValidAddress =
                    Address.validateFieldWith
                        additionalValidations
                        BillingAddressField
                        regions
                        addressData
                        |> Validation.filter [ BillingAddressField AddressType ]
                        |> Validation.isValid

                invalidState =
                    Address.validateAllWith
                        additionalValidations
                        BillingAddressField
                        addressState
                        regions
                        addressData
            in
            if isValidAddress then
                ( State { state | status = Engage.RemoteData.Loading }
                , { data | address = addressData }
                )
                    |> noCmd
                    |> return (UpdateBillingAddress addressData)

            else
                ( State { state | billingAddress = Edit invalidState addressData }, data )
                    |> noCmd
                    |> return NoOut

        View ->
            ( State state, data )
                |> noCmd
                |> return NoOut


updateAddress : Address.Msg ValidationField -> State -> BillingAddress -> ( State, BillingAddress, Cmd Msg )
updateAddress addressMsg (State state) data =
    let
        ( updatedBillingAddressState, cmd ) =
            case state.billingAddress of
                View ->
                    ( View, Cmd.none )

                Edit addressState addressData ->
                    let
                        ( updatedAddressState, updatedAddressData, cmd ) =
                            Address.update addressMsg addressState addressData
                    in
                    ( Edit updatedAddressState updatedAddressData, cmd )
    in
    ( State { state | billingAddress = updatedBillingAddressState }, data, cmd |> Cmd.map AddressMsg )


validateAll : RegionsCountry -> State -> BillingAddress -> State
validateAll regions (State state) data =
    let
        addressValidations =
            Address.validateFieldWith
                [ Validation.validateStringField (Validation.localize PhoneNumber) PhoneNumber (.phone >> .phoneNumber)
                , Validation.validateStringField (Validation.localize PhoneNumberIsoCode) PhoneNumberIsoCode (.phone >> .isoCode)
                ]
                BillingAddressField
                regions
                data.address
    in
    State
        { state
            | validations =
                Validation.validateField
                    [ Validation.validateStringField (Validation.localize FullName) FullName .fullName
                    ]
                    data
                    ++ addressValidations
        }


validateName : State -> BillingAddress -> State
validateName (State state) data =
    State
        { state
            | validations =
                Validation.validateField
                    [ Validation.validateStringField (Validation.localize FullName) FullName .fullName
                    ]
                    data
        }


isValid : State -> Bool
isValid (State state) =
    Validation.isValid state.validations && not (Error.isError state.creditCardError |> Maybe.withDefault False)


isBillingAddressValidation : ( ValidationField, Validation.ValidationStatus ) -> Bool
isBillingAddressValidation ( field, _ ) =
    case field of
        FullName ->
            False

        PhoneNumber ->
            True

        PhoneNumberIsoCode ->
            True

        BillingAddressField _ ->
            True


isEditingBillingAddress : State -> Bool
isEditingBillingAddress (State state) =
    case state.billingAddress of
        View ->
            False

        Edit _ _ ->
            True


updateCardError : String -> State -> State
updateCardError error (State state) =
    State
        { state
            | creditCardError =
                if String.isEmpty error then
                    Error.Unknown

                else
                    Error.Error { reasons = [ error ] }
            , creditCardStatus = RemoteData.NotAsked
        }
