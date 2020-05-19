module Engage.Custom.Form.Css exposing (Class(..), css)

{-| Custom.Form.Css

@docs Class

@docs css

-}

import Css exposing (..)
import Css.Foreign exposing (h2, legend, li, ul, class, descendants, Snippet)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (MessageType(Confirmation))
import Engage.Styles.Css as BaseCss
import Engage.Theme exposing (Theme)
import Engage.UI.Message.Css as MessageCss exposing (Class(..))


{-| The Class type
-}
type Class
    = Form
    | FormCompleted
    | FormTitle
    | FormCompletedContent
    | Sections
    | FieldGroup
    | File
    | FormSection


{-| Get the css
-}
css : Namespace -> Theme -> DEPRECATED.Css.File.Stylesheet
css namespace theme =
    (DEPRECATED.Css.File.stylesheet << DEPRECATED.Css.Namespace.namespace (Namespace.toString namespace))
        [ formSnippet
        , formCompletedSnippet
        , fieldGroupSnippet
        , fileSnippet
        , formSectionSnippet
        ]


formSnippet : Snippet
formSnippet =
    class Form
        [ BaseCss.normalizeMixin
        ]


formCompletedSnippet : Snippet
formCompletedSnippet =
    class FormCompleted
        [ color (hex "#585858")
        , borderBottom3 (px 2) solid (rgba 0 0 0 0.05)
        , fontSize (em 0.833)
        , margin2 (em 0.5) zero
        , padding (em 1)
        , displayFlex
        , flexDirection row
        , flexWrap wrap
        , descendants
            [ formTitleSnippet
            , formCompletedContentSnippet
            , sectionSnippet
            ]
        ]


formTitleSnippet : Snippet
formTitleSnippet =
    class FormTitle
        [ fontSize (Css.em 1.25)
        , fontWeight bold
        , margin zero
        , lineHeight (int 1)
        , flexGrow (int 1)
        , flexBasis (pct 100)
        , descendants
            [ class (MessageCss.Icon Confirmation)
                [ marginLeft (em 1)
                , verticalAlign middle
                ]
            ]
        ]


formCompletedContentSnippet : Snippet
formCompletedContentSnippet =
    class FormCompletedContent
        [ flexGrow (int 1), flexBasis (px 0) ]


sectionSnippet : Snippet
sectionSnippet =
    class Sections
        [ border2 zero none
        , displayFlex
        , justifyContent spaceBetween
        , fontSize (Css.em 0.875)
        , alignItems baseline
        , descendants
            [ ul
                [ listStyle none
                , padding4 (px 0) (px 0) (px 0) (Css.em 1)
                , margin2 (Css.em 1) (px 0)
                , flexGrow (int 1)
                ]
            , li
                [ listStyleType none
                , margin (px 0)
                , padding (px 0)
                ]
            ]
        ]


fieldGroupSnippet : Snippet
fieldGroupSnippet =
    class FieldGroup
        [ property "grid-template-columns" "repeat(auto-fit, minmax(250px,1fr))"
        , property "display" "grid"
        , property "grid-gap" "1em"
        , marginBottom (em 1)
        ]


fileSnippet : Snippet
fileSnippet =
    class File
        [ display block
        , marginBottom (Css.em 1)
        ]


formSectionSnippet : Snippet
formSectionSnippet =
    class FormSection
        [ borderStyle none
        , padding (px 0)
        , margin (px 0)
        , descendants
            [ legend
                [ padding (px 0)
                , fontSize (Css.em 1.17)
                , margin2 (Css.em 0.5) (px 0)
                , fontWeight bold
                ]
            ]
        ]
