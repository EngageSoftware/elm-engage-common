module Engage.UI.Datepicker exposing
    ( State
    , date, datepicker, initialState
    )

{-| UI.Datepicker

@docs State

@docs date, datepicker, initialState

-}

import DateTimePicker exposing (DateTime)
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
import Html.Styled exposing (toUnstyled)
import Html.Styled.Attributes exposing (fromUnstyled)


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
initialState : DateTime -> State
initialState now =
    State
        { datepicker = DateTimePicker.initialStateWithToday now
        , error = Message.initialState
        }


{-| Get the datepicker view
-}
datepicker :
    { id : String
    , onChange : State -> Maybe DateTime -> msg
    , onStateChange : State -> msg
    , labelText : String
    , requiredText : Maybe String
    , namespace : Namespace
    , status : Status
    }
    -> State
    -> Maybe DateTime
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
    , onChange : State -> Maybe DateTime -> msg
    , onStateChange : State -> msg
    , labelText : String
    , requiredText : Maybe String
    , size : Size
    , namespace : Namespace
    , status : Status
    }
    -> State
    -> Maybe DateTime
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
        , toUnstyled
            (DateTimePicker.datePicker
                (\newState -> args.onChange (State { stateData | datepicker = newState }))
                [ fromUnstyled (class [ "Datepicker-" ++ getSizeString args.size ]) ]
                stateData.datepicker
                value
            )
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
    , onChange : State -> Maybe DateTime -> msg
    , onStateChange : State -> msg
    , labelText : String
    , requiredText : Maybe String
    , namespace : Namespace
    , status : Status
    }
    -> State
    -> Maybe DateTime
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
            DateTimePicker.Config.defaultDatePickerConfig (\newState -> args.onChange (State { stateData | datepicker = newState }))
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
        (toUnstyled
            (DateTimePicker.datePickerWithConfig
                { config | usePicker = False, attributes = [ fromUnstyled (class [ "Container" ]) ] }
                [ fromUnstyled (class [ "Date-Large" ]) ]
                stateData.datepicker
                value
            )
        )


unwrap : State -> StateData
unwrap (State stateData) =
    stateData
