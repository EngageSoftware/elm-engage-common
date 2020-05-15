module Engage.Wizard exposing
    ( CompaniesStepData
    , CustomFormData
    , Form(..)
    , FormStepData
    , FormsStepData
    , InfoStepData
    , ParticipantRegistration
    , ParticipantStepData
    , RegistrationCustomFormData
    , Step(..)
    , StepData
    , StepResponse(..)
    , StepResponseData
    , WizardData
    , getCurrentForm
    , getCustomForm
    , isLoading
    , toWizardStep
    , update
    , updateCustomForm
    , updateFileUploadError
    , updateFileUploadProgress
    , updateFileUploadStatus
    , updateMembershipEventId
    )

import Dict exposing (Dict)
import Engage.Custom.Types as Custom
import Engage.Custom.Update as Custom
import Engage.Entity.Account exposing (Account)
import Engage.Entity.Address exposing (Address)
import Engage.Entity.Participant as Participant exposing (Participant)
import Engage.Entity.PhoneNumber exposing (PhoneNumber)
import Engage.Form.Company as Company
import Engage.Form.Company.Types as Company
import Engage.Form.Participant as Participant
import Engage.RemoteData exposing (WebData)
import Engage.RemoteValidation as RemoteValidation
import Engage.UI.Wizard as Wizard
import SelectDict exposing (SelectDict)



-- Types


type alias ParticipantRegistration =
    { registrationId : Maybe Int
    , participantId : Maybe Int
    , firstName : String
    , lastName : String
    , middleName : String
    , email : String
    , primaryAddress : Maybe Address
    , phone : PhoneNumber
    , mobilePhone : PhoneNumber
    , profilePicture : String
    , gender : String
    }


type alias CustomFormData =
    { formId : Int
    , formFilloutId : Int
    }


type alias RegistrationCustomFormData =
    { formId : Int
    , formFilloutId : Int
    }


type alias StepResponseData data =
    { data | registrationId : Maybe Int }


type StepResponse
    = ParticipantFormResponse (StepResponseData Participant)
    | CompaniesFormResponse (StepResponseData (Company.CompaniesData {}))
    | CustomFormResponse (StepResponseData CustomFormData)
    | RegistrationCustomFormResponse (StepResponseData RegistrationCustomFormData)
    | StepResponse (StepResponseData {})


type alias Session =
    { eventId : Int
    , participantId : Maybe Int
    }


type Step
    = Info (StepData InfoStepData)
    | Forms (StepData FormsStepData)


type alias StepData data =
    { data
        | name : String
        , id : Int
        , stepResponse : WebData StepResponse
    }


type alias InfoStepData =
    { info : String }


type alias FormsStepData =
    { forms : SelectDict Int Form
    }


type alias FormStepData =
    { form : Custom.Form }


type alias CompaniesStepData =
    { companies : Company.CompaniesData {}
    , state : Company.State
    }


type alias ParticipantStepData =
    { state : Participant.State
    }


type Form
    = ParticipantForm (StepData ParticipantStepData)
    | CompaniesForm (StepData CompaniesStepData)
    | CustomForm (StepData FormStepData)


type alias WizardData =
    { participant : Maybe Participant
    , invitedAccount : Maybe Account
    , participantCompanies : Maybe (Company.CompaniesData {})
    , registrationId : Maybe Int
    , steps : SelectDict Int Step
    , membershipEventId : Maybe Int
    }



-- Helpers


getCurrentForm : SelectDict Int Step -> Maybe Form
getCurrentForm steps =
    let
        getForm step =
            case step of
                Info _ ->
                    Nothing

                Forms formsStepData ->
                    formsStepData.forms
                        |> SelectDict.selectedValue
                        |> Just
    in
    steps
        |> SelectDict.selectedValue
        |> getForm


getCustomForm : Form -> Maybe Custom.Form
getCustomForm form =
    case form of
        ParticipantForm _ ->
            Nothing

        CompaniesForm _ ->
            Nothing

        CustomForm formStepData ->
            Just formStepData.form


toWizardStep : Step -> Wizard.Step Step Int
toWizardStep step =
    case step of
        Info stepData ->
            infoToWizardStep stepData

        Forms stepData ->
            formsToWizardStep stepData


infoToWizardStep : StepData InfoStepData -> Wizard.Step Step Int
infoToWizardStep stepData =
    Wizard.singlePage
        { title = stepData.name
        , singlePageType = Wizard.SinglePageInfo
        , status = stepData.stepResponse |> RemoteValidation.webDataToError
        , model = Info stepData
        }


formsToWizardStep : StepData FormsStepData -> Wizard.Step Step Int
formsToWizardStep stepData =
    if SelectDict.size stepData.forms > 1 then
        Wizard.multiPages
            { title = stepData.name
            , pages = stepData.forms |> SelectDict.map (always (formToPage stepData.stepResponse))
            }

    else
        Wizard.singlePage
            { title = stepData.name
            , singlePageType = Wizard.SinglePageForm
            , status = stepData.stepResponse |> RemoteValidation.webDataToError
            , model = Forms stepData
            }


formToPage : WebData StepResponse -> Form -> Wizard.Page Step
formToPage stepResponseWebData form =
    case form of
        ParticipantForm participantData ->
            { title = participantData.name
            , status = stepResponseWebData |> RemoteValidation.webDataToError
            , model = convertFormToFormsStep ParticipantForm participantData
            }

        CompaniesForm companyData ->
            { title = companyData.name
            , status = stepResponseWebData |> RemoteValidation.webDataToError
            , model = convertFormToFormsStep CompaniesForm companyData
            }

        CustomForm customForm ->
            { title = customForm.name
            , status = stepResponseWebData |> RemoteValidation.webDataToError
            , model = convertFormToFormsStep CustomForm customForm
            }


convertFormToFormsStep : (StepData fromStepData -> Form) -> StepData fromStepData -> Step
convertFormToFormsStep convertToForm formData =
    Forms
        { name = formData.name
        , id = formData.id
        , forms = SelectDict.fromDicts Dict.empty ( formData.id, convertToForm formData ) Dict.empty
        , stepResponse = formData.stepResponse
        }


update : Custom.ChangeArgs -> Custom.Answer -> Step -> Step
update args answer stepType =
    case stepType of
        Forms formStepData ->
            Forms
                { formStepData
                    | forms = SelectDict.updateSelected (always <| updateForm args answer) formStepData.forms
                }

        _ ->
            stepType


updateMembershipEventId : Int -> Step -> Step
updateMembershipEventId membershipEventId step =
    let
        updater form =
            case form of
                CustomForm data ->
                    CustomForm { data | form = Custom.updateMembershipEventId membershipEventId data.form }

                _ ->
                    form
    in
    case step of
        Forms stepData ->
            Forms { stepData | forms = SelectDict.map (\_ form -> updater form) stepData.forms }

        _ ->
            step


updateForm : Custom.ChangeArgs -> Custom.Answer -> Form -> Form
updateForm args answer form =
    case form of
        CustomForm customForm ->
            Custom.updateAnswer args args.fieldType answer customForm.form
                |> (\form -> CustomForm { customForm | form = form })

        ParticipantForm _ ->
            form

        CompaniesForm _ ->
            form


updateCustomForm : (Custom.Form -> Custom.Form) -> Step -> Step
updateCustomForm updater step =
    let
        formUpdateHelper _ form =
            case form of
                ParticipantForm _ ->
                    form

                CompaniesForm _ ->
                    form

                CustomForm formStepData ->
                    CustomForm { formStepData | form = updater formStepData.form }
    in
    case step of
        Info _ ->
            step

        Forms formsStepData ->
            Forms { formsStepData | forms = SelectDict.updateSelected formUpdateHelper formsStepData.forms }


isLoading : Step -> Bool
isLoading step =
    case step of
        Info stepData ->
            stepData.stepResponse |> Engage.RemoteData.isLoading

        Forms stepData ->
            stepData.stepResponse |> Engage.RemoteData.isLoading


updateFileUploadProgress : Custom.FileUploadProgress -> Step -> Step
updateFileUploadProgress fileUploadProgress step =
    updateCustomForm (Custom.updateFileUploadProgress fileUploadProgress) step


updateFileUploadError : Custom.FileUploadError -> Step -> Step
updateFileUploadError fileUploadError step =
    updateCustomForm (Custom.updateFileUploadError fileUploadError) step


updateFileUploadStatus : Custom.FileUploadStatus -> Step -> Step
updateFileUploadStatus fileUploadStatus step =
    updateCustomForm (Custom.updateFileUploadStatus fileUploadStatus) step
