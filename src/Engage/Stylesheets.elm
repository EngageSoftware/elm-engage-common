module Engage.Stylesheets exposing (stylesheets)

{-| Stylesheets

@docs stylesheets

-}

import Css
import DEPRECATED.Css.File
import DateTimePicker.Config
import DateTimePicker.Css
import Engage.Custom.Form.Css
import Engage.Form.CreditCard.Css
import Engage.Form.FormAction.Css
import Engage.Form.Participant.Css
import Engage.Form.Profile.Css
import Engage.Namespace as Namespace
import Engage.Pattern.CardCollector.Css
import Engage.Styles.Css
import Engage.Theme as Theme
import Engage.UI.Accordion.Css
import Engage.UI.Button.Css
import Engage.UI.Card.Css
import Engage.UI.Datepicker.Css
import Engage.UI.Dialog.Css
import Engage.UI.Dropdown.Css
import Engage.UI.Info.Css
import Engage.UI.Input.Css
import Engage.UI.Loading.Css
import Engage.UI.Message.Css
import Engage.UI.PictureUpload.Css
import Engage.UI.Tooltip.Css
import Engage.UI.Wizard.Css
import IntlPhoneInput.Css


{-| Get the stylesheets from a theme
-}
stylesheets : Theme.Theme -> List DEPRECATED.Css.File.Stylesheet
stylesheets theme =
    [ DateTimePicker.Css.cssWithNamespace DateTimePicker.Config.defaultNamespace
    , IntlPhoneInput.Css.css <| Namespace.toString Namespace.engagecore
    , Engage.Styles.Css.css Namespace.engagecore
    , Engage.UI.Info.Css.css Namespace.engagecore theme
    , Engage.UI.Input.Css.css Namespace.engagecore theme
    , Engage.UI.Tooltip.Css.css Namespace.engagecore theme
    , Engage.UI.Dropdown.Css.css Namespace.engagecore theme
    , Engage.UI.Datepicker.Css.css Namespace.engagecore theme
    , Engage.UI.Button.Css.css Namespace.engagecore theme
    , Engage.UI.Message.Css.css Namespace.engagecore theme
    , Engage.UI.Loading.Css.css Namespace.engagecore theme
    , Engage.UI.Wizard.Css.css Namespace.engagecore theme
    , Engage.UI.Accordion.Css.css Namespace.engagecore theme
    , Engage.UI.Card.Css.css Namespace.engagecore theme
    , Engage.UI.PictureUpload.Css.css Namespace.engagecore theme
    , Engage.UI.Dialog.Css.css Namespace.engagecore theme
    , Engage.Pattern.CardCollector.Css.css Namespace.engagecore theme
    , Engage.Form.Profile.Css.css Namespace.engagecore theme
    , Engage.Custom.Form.Css.css Namespace.engagecore theme
    , Engage.Form.CreditCard.Css.css Namespace.engagecore theme
    , Engage.Form.Participant.Css.css Namespace.engagecore theme
    , Engage.Form.FormAction.Css.css Namespace.engagecore theme
    ]
