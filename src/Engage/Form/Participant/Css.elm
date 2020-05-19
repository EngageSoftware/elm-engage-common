module Engage.Form.Participant.Css exposing (Class(..), css)

{-| Form.Participant.Css

@docs Class

@docs css

-}

import Css exposing (..)
import Css.Foreign exposing (Snippet, class)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Css as BaseCss
import Engage.Styles.MediaQuery as MediaQuery
import Engage.Theme as Theme exposing (Theme)


{-| The Class type
-}
type Class
    = Participant
    | ParticipantAccount
    | ParticipantTitle
    | ParticipantPicture
    | ParticipantForm


{-| Get the css
-}
css : Namespace -> Theme -> DEPRECATED.Css.File.Stylesheet
css namespace theme =
    (DEPRECATED.Css.File.stylesheet << DEPRECATED.Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ participantSnippet
    , participantAccountSnippet
    , largeParticipantSnippet
    , titleSnippet
    , largeTitleSnippet
    , pictureSnippet
    , formSnippet
    ]


participantSnippet : Snippet
participantSnippet =
    class Participant
        [ BaseCss.normalizeMixin
        , displayFlex
        , flexDirection column
        ]


participantAccountSnippet : Snippet
participantAccountSnippet =
    class ParticipantAccount
        [ BaseCss.normalizeMixin
        , displayFlex
        , flexDirection column
        ]


largeParticipantSnippet : Snippet
largeParticipantSnippet =
    MediaQuery.atMedia MediaQuery.Medium
        [ class Participant
            [ property "grid-template-columns" "[side] 300px [main] 1fr"
            , property "display" "grid"
            , property "grid-gap" "1.5em"
            ]
        ]


titleSnippet : Snippet
titleSnippet =
    class ParticipantTitle
        [ BaseCss.normalizeMixin
        ]


largeTitleSnippet : Snippet
largeTitleSnippet =
    MediaQuery.atMedia MediaQuery.Medium
        [ class ParticipantTitle
            [ BaseCss.normalizeMixin
            , property "grid-column" "side / main-end"
            ]
        ]


pictureSnippet : Snippet
pictureSnippet =
    class ParticipantPicture
        [ BaseCss.normalizeMixin
        , minWidth (px 300)
        , marginBottom (em 2)
        , marginLeft auto
        , marginRight auto
        ]


formSnippet : Snippet
formSnippet =
    class ParticipantForm
        [ BaseCss.normalizeMixin
        ]
