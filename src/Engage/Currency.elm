module Engage.Currency exposing (format)

import Engage.Localization as Localization exposing (Localization)
import Language exposing (Language)
import Numeral


format : { a | localization : Localization } -> Language -> Float -> String
format args currency amount =
    Numeral.formatWithLanguage currency (Localization.localizeStringWithDefault "$0,0.00" "USD" args) amount
