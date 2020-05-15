module Engage.Custom.Types exposing
    ( Answer(..)
    , AnswerData
    , BoolEntryData
    , ChangeArgs
    , Config
    , Disable(..)
    , Entry(..)
    , EntryData
    , Field
    , FieldChoice
    , FieldGroup
    , FieldType(..)
    , FileEntryData
    , FileStatus(..)
    , FileUploadError
    , FileUploadProgress
    , FileUploadStatus
    , Form
    , Level(..)
    , MultipleEntryData
    , Section
    , StaticFormType(..)
    , UpdateOptions(..)
    , defaultCompanyForm
    , defaultConfig
    , defaultParticipantForm
    )

import Date
import Date.Extra.Config.Config_en_us as Config
import Date.Extra.Format
import Dict exposing (Dict)
import Engage.Entity.Address as Address
import Engage.Form.MembershipTypeList as MembershipTypeList exposing (MembershipType)
import Engage.Localization as Localization
import Engage.UI.Accordion as Accordion
import Engage.UI.Datepicker as Datepicker
import Engage.UI.Dropdown as Dropdown
import Engage.UI.Input as Input
import Engage.Validation as Validation exposing (ValidationErrors)
import Set exposing (Set)


type alias Form =
    { formId : Int
    , formFilloutId : Maybe Int
    , name : String
    , sections : Dict Int Section
    , formLevel : Level
    , relativeOrder : Int
    , validations : ValidationErrors { fieldId : Int }
    }


defaultParticipantForm : Form
defaultParticipantForm =
    { formId = 0
    , formFilloutId = Nothing
    , name = ""
    , sections = Dict.empty
    , formLevel = Participant
    , relativeOrder = 0
    , validations = []
    }


defaultCompanyForm : Form
defaultCompanyForm =
    { formId = 0
    , formFilloutId = Nothing
    , name = ""
    , sections = Dict.empty
    , formLevel = Company
    , relativeOrder = 0
    , validations = []
    }


type Level
    = Participant
    | Registration
    | Company
    | Additional


type alias Section =
    { sectionId : Int
    , name : String
    , relativeOrder : Int
    , optional : Bool
    , optionalLabel : String
    , adminOnly : Bool
    , fieldGroups : Dict Int FieldGroup
    }


type Answer
    = Answer AnswerData
    | MultipleAnswer (Set String)
    | BoolAnswer Bool
    | FileAnswer Input.FileInfo
    | MembershipTypeAnswer (Maybe MembershipTypeList.MembershipType)


type alias AnswerData =
    { value : String }


type alias Field =
    { fieldId : Int
    , relativeOrder : Int
    , label : String
    , description : String
    , fieldType : FieldType
    , required : Bool
    , errorMessage : String
    , disable : Disable
    , valueMin : String
    , valueMax : String
    , valueStep : String
    , updateOptions : UpdateOptions
    }


type alias FieldGroup =
    { fieldGroupId : Int
    , fields : Dict Int Field
    , relativeOrder : Int
    }


type UpdateOptions
    = AlwaysUpdate
    | Update
    | DontUpdate


type alias FieldChoice =
    { fieldChoiceId : Maybe Int
    , name : String
    , value : String
    , relativeOrder : Int
    }


type Entry
    = Entry EntryData
    | BoolEntry BoolEntryData
    | FileEntry FileEntryData


type alias EntryData =
    { value : String
    }


type alias MultipleEntryData =
    { values : Set String
    }


type alias BoolEntryData =
    { value : Bool
    }


type alias FileEntryData =
    { name : String
    , fileType : String
    , status : FileStatus
    }


type FileStatus
    = NoFile
    | Uploading { progressPercentage : Float }
    | Uploaded
    | Error { message : String }


type FieldType
    = TextBox { entry : EntryData, state : Input.State }
    | LargeTextBox { entry : EntryData, state : Input.State }
    | TextArea { entry : EntryData, state : Input.State }
    | CheckBox { entry : BoolEntryData, state : Input.State }
    | DropDown { entry : EntryData, state : Dropdown.State, fieldChoices : List FieldChoice }
    | RadioList { entry : EntryData, state : Input.State, fieldChoices : List FieldChoice }
    | CheckBoxList { entry : MultipleEntryData, state : Input.State, fieldChoices : List FieldChoice }
    | Quantity { entry : EntryData, state : Input.State }
    | Date { entry : EntryData, state : Datepicker.State }
    | Email
    | Phone
    | ZipCode
    | USState
    | File { entry : FileEntryData, state : Input.State }
    | Region { entry : EntryData, state : Dropdown.State }
    | Country { entry : EntryData, state : Dropdown.State }
    | Text
    | StaticForm StaticFormType


type StaticFormType
    = ParticipantForm
    | MembershipTypeList { state : Accordion.State, membershipTypeList : List MembershipType, entry : Maybe MembershipType }


type Disable
    = None
    | Disabled
    | Hidden


defaultConfig :
    { onChange : ChangeArgs -> Answer -> msg
    , onEnter : msg
    , onGotoPage : { pageId : Int } -> msg
    , localization : Localization.Localization
    }
    -> Config msg
defaultConfig { onChange, onEnter, onGotoPage, localization } =
    { onChange = onChange
    , onEnter = onEnter
    , dateFormatter = Date.Extra.Format.format Config.config "%m/%d/%Y"
    , onGotoPage = onGotoPage
    , localization = localization
    , countries = Dict.empty
    , regions = Dict.empty
    }


type alias Config msg =
    { onChange : ChangeArgs -> Answer -> msg
    , onEnter : msg
    , dateFormatter : Date.Date -> String
    , onGotoPage : { pageId : Int } -> msg
    , localization : Localization.Localization
    , countries : Address.Countries
    , regions : Address.RegionsCountry
    }


type alias ChangeArgs =
    { formId : Int
    , sectionId : Int
    , fieldGroupId : Int
    , fieldId : Int
    , fieldType : FieldType
    , domId : String
    , onlyStateChange : Bool
    }


type alias FileUploadProgress =
    { formId : Int
    , sectionId : Int
    , fieldGroupId : Int
    , fieldId : Int
    , progressPercentage : Float
    }


type alias FileUploadStatus =
    { formId : Int
    , sectionId : Int
    , fieldGroupId : Int
    , fieldId : Int
    , formFilloutId : Int
    }


type alias FileUploadError =
    { formId : Int
    , sectionId : Int
    , fieldGroupId : Int
    , fieldId : Int
    , errorMessage : String
    }
