module Engage.Custom.Types exposing
    ( Answer(..), AnswerData, BoolEntryData, ChangeArgs, Config, Disable(..), Entry(..), EntryData, Field, FieldChoice, FieldGroup, FieldType(..), FileEntryData, FileStatus(..), FileUploadError, FileUploadProgress, FileUploadStatus, Form, Level(..), MultipleEntryData, Section, StaticFormType(..), UpdateOptions(..)
    , defaultCompanyForm, defaultConfig, defaultParticipantForm
    )

{-| Custom.Types

@docs Answer, AnswerData, BoolEntryData, ChangeArgs, Config, Disable, Entry, EntryData, Field, FieldChoice, FieldGroup, FieldType, FileEntryData, FileStatus, FileUploadError, FileUploadProgress, FileUploadStatus, Form, Level, MultipleEntryData, Section, StaticFormType, UpdateOptions

@docs defaultCompanyForm, defaultConfig, defaultParticipantForm

-}

import Dict exposing (Dict)
import Engage.Entity.Address as Address
import Engage.Form.MembershipTypeList as MembershipTypeList exposing (MembershipType)
import Engage.Localization as Localization
import Engage.UI.Accordion as Accordion
import Engage.UI.Datepicker as Datepicker
import Engage.UI.Dropdown as Dropdown
import Engage.UI.Input as Input
import Engage.Validation as Validation exposing (ValidationResult)
import Set exposing (Set)
import Time
import Time.Format
import Time.Format.Config.Config_en_us as Config


{-| The Form type
-}
type alias Form =
    { formId : Int
    , formFilloutId : Maybe Int
    , name : String
    , sections : Dict Int Section
    , formLevel : Level
    , relativeOrder : Int
    , validations : ValidationResult { fieldId : Int }
    }


{-| Get the default Participant Form
-}
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


{-| Get the default Company Form
-}
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


{-| The Level type
-}
type Level
    = Participant
    | Registration
    | Company
    | Additional


{-| The Section type
-}
type alias Section =
    { sectionId : Int
    , name : String
    , relativeOrder : Int
    , optional : Bool
    , optionalLabel : String
    , adminOnly : Bool
    , fieldGroups : Dict Int FieldGroup
    }


{-| The Answer type
-}
type Answer
    = Answer AnswerData
    | MultipleAnswer (Set String)
    | BoolAnswer Bool
    | FileAnswer Input.FileInfo
    | MembershipTypeAnswer (Maybe MembershipTypeList.MembershipType)


{-| The AnswerData type
-}
type alias AnswerData =
    { value : String }


{-| The Field type
-}
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


{-| The FieldGroup type
-}
type alias FieldGroup =
    { fieldGroupId : Int
    , fields : Dict Int Field
    , relativeOrder : Int
    }


{-| The UpdateOptions type
-}
type UpdateOptions
    = AlwaysUpdate
    | Update
    | DontUpdate


{-| The FieldChoice type
-}
type alias FieldChoice =
    { fieldChoiceId : Maybe Int
    , name : String
    , value : String
    , relativeOrder : Int
    }


{-| The Entry type
-}
type Entry
    = Entry EntryData
    | BoolEntry BoolEntryData
    | FileEntry FileEntryData


{-| The EntryData type
-}
type alias EntryData =
    { value : String
    }


{-| The MultipleEntryData type
-}
type alias MultipleEntryData =
    { values : Set String
    }


{-| The BoolEntryData type
-}
type alias BoolEntryData =
    { value : Bool
    }


{-| The FileEntryData type
-}
type alias FileEntryData =
    { name : String
    , fileType : String
    , status : FileStatus
    }


{-| The FileStatus type
-}
type FileStatus
    = NoFile
    | Uploading { progressPercentage : Float }
    | Uploaded
    | Error { message : String }


{-| The FieldType type
-}
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


{-| The StaticFormType type
-}
type StaticFormType
    = ParticipantForm
    | MembershipTypeList { state : Accordion.State, membershipTypeList : List MembershipType, entry : Maybe MembershipType }


{-| The Disable type
-}
type Disable
    = None
    | Disabled
    | Hidden


{-| Get the default Config
-}
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
    , dateFormatter = Time.Format.format Config.config "%m/%d/%Y"
    , onGotoPage = onGotoPage
    , localization = localization
    , countries = Dict.empty
    , regions = Dict.empty
    }


{-| The Config type
-}
type alias Config msg =
    { onChange : ChangeArgs -> Answer -> msg
    , onEnter : msg
    , dateFormatter : Time.Zone -> Time.Posix -> String
    , onGotoPage : { pageId : Int } -> msg
    , localization : Localization.Localization
    , countries : Address.Countries
    , regions : Address.RegionsCountry
    }


{-| The ChargeArgs type
-}
type alias ChangeArgs =
    { formId : Int
    , sectionId : Int
    , fieldGroupId : Int
    , fieldId : Int
    , fieldType : FieldType
    , domId : String
    , onlyStateChange : Bool
    }


{-| The FileUploadProgress type
-}
type alias FileUploadProgress =
    { formId : Int
    , sectionId : Int
    , fieldGroupId : Int
    , fieldId : Int
    , progressPercentage : Float
    }


{-| The FileUploadStatus type
-}
type alias FileUploadStatus =
    { formId : Int
    , sectionId : Int
    , fieldGroupId : Int
    , fieldId : Int
    , formFilloutId : Int
    }


{-| The FileUploadError type
-}
type alias FileUploadError =
    { formId : Int
    , sectionId : Int
    , fieldGroupId : Int
    , fieldId : Int
    , errorMessage : String
    }
