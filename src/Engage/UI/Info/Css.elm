module Engage.UI.Info.Css exposing
    ( Class(..)
    , css
    )

import Css exposing (..)
import Css.Namespace
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)
import Engage.UI.Svg.Css exposing (Class(SvgBool, SvgFax, SvgMail, SvgMobilePhone, SvgPhone))


type Class
    = Info
    | InfoTitle
    | InfoContent
    | InfoIcon
    | InfoMail
    | InfoPhone
    | InfoMobilePhone
    | InfoFax
    | InfoGroup
    | InfoBool


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class Info
        [ infoMixin theme ]
    , class InfoTitle
        [ infoTitleMixin theme ]
    , class InfoContent
        [ infoContentMixin theme ]
    , class InfoMail
        [ infoInlineMixin theme SvgMail ]
    , class InfoPhone
        [ infoInlineMixin theme SvgPhone ]
    , class InfoMobilePhone
        [ infoInlineMixin theme SvgMobilePhone ]
    , class InfoFax
        [ infoInlineMixin theme SvgFax ]
    , class InfoBool
        [ infoInlineMixin theme SvgBool ]
    , class InfoGroup
        [ infoMixin theme ]
    ]


infoMixin : Theme -> Mixin
infoMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        , marginBottom (em 1)
        ]


infoTitleMixin : Theme -> Mixin
infoTitleMixin theme =
    mixin
        [ BaseCss.normalizeMixin
        , fontWeight bold
        ]


infoContentMixin : Theme -> Mixin
infoContentMixin theme =
    mixin [ BaseCss.normalizeMixin ]


infoInlineMixin : Theme -> svgClass -> Mixin
infoInlineMixin theme svgClass =
    mixin
        [ BaseCss.normalizeMixin
        , displayFlex
        , flexDirection row
        , alignItems center
        , marginBottom zero
        , descendants [ class svgClass [ marginRight (em 0.5) ] ]
        ]
