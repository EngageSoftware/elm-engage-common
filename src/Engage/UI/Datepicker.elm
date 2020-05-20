module Engage.UI.Datepicker exposing
    ( State
    , date, datepicker, initialState
    )

{-| UI.Datepicker

@docs State

@docs date, datepicker, initialState

-}

import Date exposing (Date)
import DateTimePicker
import DateTimePicker.Config
import Engage.CssHelpers
import Engage.Html.Extra as HtmlExtra
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Class(..), Size(..), getSizeString)
import Engage.UI.Error as Error exposing (Status(..))
import Engage.UI.FormControl as FormControl
import Engage.UI.Message as Message
import Html exposing (..)
import Html.Attributes exposing (..)


{-| The State type
-}
type State
    = State StateData


type alias StateData =
    { datepicker : DateTimePicker.State
    , error : Message.State
    }


{-| Get the initial State
-}
initialState : Date -> State
initialState now =
    State
        { datepicker = DateTimePicker.initialStateWithToday now
        , error = Message.initialState
        }


{-| Get the datepicker view
-}
datepicker :
    { id : String
    , onChange : State -> Maybe Date -> msg
    , onStateChange : State -> msg
    , labelText : String
    , requiredText : Maybe String
    , namespace : Namespace
    , status : Status
    }
    -> State
    -> Maybe Date
    -> Html msg
datepicker args state value =
    datepickerWithSize
        { id = args.id
        , onChange = args.onChange
        , onStateChange = args.onStateChange
        , labelText = args.labelText
        , requiredText = args.requiredText
        , size = Large
        , namespace = args.namespace
        , status = args.status
        }
        state
        value


datepickerWithSize :
    { id : String
    , onChange : State -> Maybe Date -> msg
    , onStateChange : State -> msg
    , labelText : String
    , requiredText : Maybe String
    , size : Size
    , namespace : Namespace
    , status : Status
    }
    -> State
    -> Maybe Date
    -> Html msg
datepickerWithSize args state value =
    let
        class =
            args.namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        requiredIndicator =
            args.requiredText
                |> Maybe.map (\required -> span [ class [ "Required" ], title required ] [ text "*" ])
                |> Maybe.withDefault HtmlExtra.none

        stateData =
            unwrap state

        onValidationStateChange errorState =
            args.onStateChange (State { stateData | error = errorState })
    in
    div [ class [ "FormControl-" ++ getSizeString args.size ] ]
        [ label
            [ class [ "Label" ], for args.id ]
            [ text args.labelText, requiredIndicator ]
        , DateTimePicker.datePicker
            (\state -> args.onChange (State { stateData | datepicker = state }))
            [ class [ "Datepicker-" ++ getSizeString args.size ] ]
            stateData.datepicker
            value
        , Error.inlineError
            { namespace = args.namespace
            , status = args.status
            , onChange = onValidationStateChange
            }
            stateData.error
        ]


{-| Get the date view
-}
date :
    { id : String
    , onChange : State -> Maybe Date -> msg
    , onStateChange : State -> msg
    , labelText : String
    , requiredText : Maybe String
    , namespace : Namespace
    , status : Status
    }
    -> State
    -> Maybe Date
    -> Html msg
date args state value =
    let
        class =
            args.namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        stateData =
            unwrap state

        onValidationStateChange errorState =
            args.onStateChange (State { stateData | error = errorState })

        config =
            DateTimePicker.Config.defaultDatePickerConfig (\state -> args.onChange (State { stateData | datepicker = state }))
    in
    FormControl.formControl
        { namespace = args.namespace
        , size = Large
        , id = args.id
        , labelText = args.labelText
        , helpText = ""
        , requiredText = args.requiredText
        , status = args.status
        , onValidationStateChange = onValidationStateChange
        }
        stateData.error
        (DateTimePicker.datePickerWithConfig
            { config | usePicker = False, attributes = [ class [ "Container" ] ] }
            [ class [ "Date-Large" ] ]
            stateData.datepicker
            value
        )


unwrap : State -> StateData
unwrap (State stateData) =
    stateData
