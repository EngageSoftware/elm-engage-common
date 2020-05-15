module Engage.Form.HideOrShow exposing (HideOrShow, Visibility(..), fromHideBool, fromShowBool, showAll)


type alias HideOrShow =
    { birthDate : String
    , birthDateYears : List Int
    , birthDateMonths : List String
    , birthDateRequired : Bool
    , gender : Visibility
    , account : Visibility
    , fax : Visibility
    , website : Visibility
    , headshot : Visibility
    , addressTypeId : Maybe Int
    , addressPhone : Visibility
    , addressName : Visibility
    }


type Visibility
    = Hide
    | Show
    | ReadOnly


showAll : HideOrShow
showAll =
    { birthDate = ""
    , birthDateYears = []
    , birthDateMonths = []
    , birthDateRequired = True
    , gender = Show
    , account = Show
    , fax = Show
    , website = Show
    , headshot = Show
    , addressTypeId = Nothing
    , addressPhone = Show
    , addressName = Show
    }


fromShowBool : Bool -> Visibility
fromShowBool show =
    if show then
        Show

    else
        Hide


fromHideBool : Bool -> Visibility
fromHideBool hide =
    if hide then
        Hide

    else
        Show
