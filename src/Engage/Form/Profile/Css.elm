module Engage.Form.Profile.Css exposing
    ( Class(..)
    , css
    )

import Css exposing (..)
import Css.Foreign exposing (Snippet, class, descendants)
import DEPRECATED.Css.Namespace
import DEPRECATED.Css.File
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Css as BaseCss
import Engage.Styles.MediaQuery as MediaQuery
import Engage.Theme as Theme exposing (Theme)
import Engage.ThemeHelper as Theme


type Class
    = Profile
    | ProfileHeader
    | ProfileInfo
    | ProfileBody
    | ProfileAvatar
    | ProfileName
    | ProfileTitle
    | ProfileEmail
    | ProfilePhone
    | ProfileCellphone
    | ProfileFax
    | ProfileEditButton
    | ProfileEditAccountLink
    | ProfileNoAvatar
    | ProfileGender


css : Namespace -> Theme -> DEPRECATED.Css.File.Stylesheet
css namespace theme =
    (DEPRECATED.Css.File.stylesheet << DEPRECATED.Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    let
        themePalette =
            Theme.palette theme
    in
    [ class Profile [ profileMixin theme ]
    , MediaQuery.atMedia MediaQuery.Medium
        [ class Profile [ largeProfileMixin theme ] ]
    , class ProfileHeader [ profileHeaderMixin theme ]
    , class ProfileAvatar [ profileAvatarMixin theme ]
    , class ProfileNoAvatar [ descendants [ Css.Foreign.img [ property "opacity" "1" ] ] ]
    , class ProfileInfo [ width (pct 100) ]
    , class ProfileEditAccountLink [ displayFlex, alignItems center ]
    ]


profileMixin : Theme -> Style
profileMixin theme =
    batch
        [ BaseCss.normalizeMixin
        , position relative
        , displayFlex
        , flexDirection column
        ]


largeProfileMixin : Theme -> Style
largeProfileMixin theme =
    batch [ flexDirection row ]


profileHeaderMixin : Theme -> Style
profileHeaderMixin theme =
    batch
        [ BaseCss.normalizeMixin
        , displayFlex
        , flexDirection column
        , marginBottom (em 1)
        ]


profileAvatarMixin : Theme -> Style
profileAvatarMixin theme =
    batch
        [ BaseCss.normalizeMixin
        , width (px 300)
        , descendants [ Css.Foreign.img [ width (pct 100) ] ]
        , marginRight (em 1)
        , marginBottom (em 1)
        ]
