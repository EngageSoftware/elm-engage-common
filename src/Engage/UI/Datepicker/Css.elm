module Engage.UI.Datepicker.Css exposing
    ( Class(..)
    , css
    , snippets
    )

import Css exposing (..)
import Css.Namespace
import DateTimePicker.SharedStyles as DateTimePickerCss
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Class(..), Size(..))
import Engage.Theme as Theme exposing (Theme)
import Engage.UI.Input.Css exposing (formControlMixin, inputMixin, labelMixin, largeMixin, smallMixin)


type Class
    = Datepicker Size
    | Container
    | Date Size
    | Label


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class (FormControl Large)
        [ descendants
            [ class Label [ labelMixin theme Large ]
            , class (Date Large)
                [ inputMixin theme
                , largeMixin theme
                ]
            , class (Datepicker Large)
                [ inputMixin theme
                , largeMixin theme
                ]
            , class Container [ width (pct 100) ]
            , dateTimePickerOverride
            ]
        ]
    , class (FormControl Small)
        [ descendants
            [ class Label [ labelMixin theme Small ]
            , class (Date Small)
                [ inputMixin theme
                , smallMixin theme
                ]
            , class (Datepicker Small)
                [ inputMixin theme
                , smallMixin theme
                ]
            , class Container [ width (pct 100) ]
            , dateTimePickerOverride
            ]
        ]
    ]


dateTimePickerOverride : Snippet
dateTimePickerOverride =
    class DateTimePickerCss.DatePicker
        [ width (pct 100)
        , descendants
            [ class DateTimePickerCss.Dialog
                [ zIndex (int 10000)
                , descendants
                    [ class DateTimePickerCss.Footer
                        [ height auto
                        ]
                    ]
                ]
            ]
        ]
