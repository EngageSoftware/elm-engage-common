module Engage.Form.HideOrShow exposing (HideOrShow, Visibility(..), fromHideBool, fromShowBool, showAll)


{-| HideOrShow

@docs HideOrShow, Visibility

@docs fromHideBool, fromShowBool, showAll

-}


{-| The HideOrShow type
-}
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


{-| The Visibility type
-}
type Visibility
    = Hide
    | Show
    | ReadOnly



{-| Show all
-}
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


{-| Convert a show bool into a Visibility
-}
fromShowBool : Bool -> Visibility
fromShowBool show =
    if show then
        Show

    else
        Hide


{-| Convert a hide bool into a Visibility
-}
fromHideBool : Bool -> Visibility
fromHideBool hide =
    if hide then
        Hide

    else
        Show
