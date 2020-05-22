module Engage.UI.Datepicker exposing
    ( State
    , date, datepicker, initialState, toDateTime
    )

{-| UI.Datepicker

@docs State

@docs date, datepicker, initialState, toDateTime

-}

import Array
import Date exposing (Date)
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
    { datepicker : Maybe DateTimePicker.State
    , error : Message.State
    }


{-| Get the initial State
-}
initialState : Date -> State
initialState now =
    let
        convertedNow =
            toDateTime (Just now)
    in
    case convertedNow of
        Just nowDate ->
            State
                { datepicker = Just (DateTimePicker.initialStateWithToday nowDate)
                , error = Message.initialState
                }

        Nothing ->
            State
                { datepicker = Nothing
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
        , case stateData.datepicker of
            Just datepickerState ->
                toUnstyled
                    (DateTimePicker.datePicker
                        (\newState dateTime -> args.onChange (State { stateData | datepicker = Just newState }) (fromDateTime dateTime))
                        [ fromUnstyled (class [ "Datepicker-" ++ getSizeString args.size ]) ]
                        datepickerState
                        (toDateTime value)
                    )

            Nothing ->
                text ""
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
            DateTimePicker.Config.defaultDatePickerConfig (\newState dateTime -> args.onChange (State { stateData | datepicker = Just newState }) (fromDateTime dateTime))
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
        (case stateData.datepicker of
            Just datePickerState ->
                toUnstyled
                    (DateTimePicker.datePickerWithConfig
                        { config | usePicker = False, attributes = [ fromUnstyled (class [ "Container" ]) ] }
                        [ fromUnstyled (class [ "Date-Large" ]) ]
                        datePickerState
                        (toDateTime value)
                    )

            Nothing ->
                text ""
        )


unwrap : State -> StateData
unwrap (State stateData) =
    stateData


{-| Convert a Date to a DateTime
-}
toDateTime : Maybe Date -> Maybe DateTime
toDateTime oldDate =
    case oldDate of
        Just dateValue ->
            dateValue
                |> Date.format "M/d/Y"
                |> DateTimePicker.Config.defaultDateFromInput

        Nothing ->
            Nothing


fromDateTime : Maybe DateTime -> Maybe Date
fromDateTime oldDateTime =
    case oldDateTime of
        Just dateTimeValue ->
            let
                dateParts =
                    dateTimeValue
                        |> DateTimePicker.Config.defaultDateToInput
                        |> String.split "/"
                        |> Array.fromList

                month =
                    dateParts
                        |> Array.get 0
                        |> Maybe.withDefault ""

                day =
                    dateParts
                        |> Array.get 1
                        |> Maybe.withDefault ""

                year =
                    dateParts
                        |> Array.get 2
                        |> Maybe.withDefault ""

                isoString =
                    year ++ "-" ++ month ++ "-" ++ day
            in
            isoString
                |> Date.fromIsoString
                |> Result.toMaybe

        Nothing ->
            Nothing
