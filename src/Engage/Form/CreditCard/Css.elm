module Engage.Form.CreditCard.Css exposing (Class(..), Visibility(..), css, toVisibility)

import Css exposing (..)
import Css.Foreign exposing (legend, class, descendants, Snippet)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Custom.Form.Css as CustomFormCss
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Css as BaseCss
import Engage.Styles.MediaQuery exposing (BreakPoint(..), atMedia)
import Engage.Theme as Theme exposing (Theme)


type Class
    = CreditCard
    | CreditCardSection
    | BillingAddressSection
    | CreditCardForm
    | BillingAddressForm Visibility
    | CompletedBillingAddress Visibility
    | Card
    | FormTitle


type Visibility
    = Visible
    | Hidden


toVisibility : Bool -> Visibility
toVisibility bool =
    if bool then
        Visible

    else
        Hidden


css : Namespace -> Theme -> DEPRECATED.Css.File.Stylesheet
css namespace theme =
    (DEPRECATED.Css.File.stylesheet << DEPRECATED.Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class BillingAddressSection
        [ position relative
        ]
    , class CreditCard
        [ BaseCss.normalizeMixin
        , descendants
            [ class CreditCardSection
                [ displayFlex
                , flexDirection column
                , descendants
                    [ class FormTitle
                        [ order (int 0)
                        , flexGrow (int 1)
                        , flexShrink (int 1)
                        , flexBasis (pct 100)
                        ]
                    , class CreditCardForm
                        [ flexGrow (int 1)
                        , flexBasis (pct 50)
                        , order (int 2)
                        ]
                    , class Card
                        [ order (int 1)
                        , padding2 (em 2) (px 0)
                        , flexBasis (pct 50)
                        , flexGrow (int 1)
                        , displayFlex
                        , justifyContent center
                        , descendants
                            [ Css.Foreign.svg
                                [ maxWidth (px 350)
                                , width (pct 100)
                                , height (pct 100)
                                ]
                            ]
                        ]
                    , class CustomFormCss.FieldGroup
                        [ property "grid-template-columns" "repeat(auto-fit, minmax(70px,1fr))"
                        , property "display" "grid"
                        , property "grid-gap" "1em"
                        ]
                    ]
                ]
            , class (CompletedBillingAddress Hidden)
                [ display none
                ]
            , class (CompletedBillingAddress Visible)
                [ display block
                , descendants
                    [ class CustomFormCss.FormCompleted
                        [ borderBottom2 zero none ]
                    ]
                ]
            , class (BillingAddressForm Hidden)
                [ opacity (num 0)
                , transform (scaleY 0)
                , property "transform-origin" "top"
                , height zero
                ]
            , class (BillingAddressForm Visible)
                [ opacity (num 1)
                , transform (scaleY 1)
                , property "transform-origin" "top"
                , property "transition" "transform .2s"
                , height (pct 100)
                ]
            ]
        ]
    , atMedia Medium
        [ class CreditCard
            [ descendants
                [ class CreditCardSection
                    [ flexDirection row
                    , flexWrap wrap
                    , descendants
                        [ class FormTitle [ order (int 0) ]
                        , class CreditCardForm [ order (int 1) ]
                        , class Card
                            [ order (int 2), padding (px 0), paddingLeft (em 2) ]
                        ]
                    ]
                ]
            ]
        ]
    ]
