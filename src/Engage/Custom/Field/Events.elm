module Engage.Custom.Field.Events exposing (onChangeHandler, onCheckHandler, onDateChangeHandler, onFileSelectHandler, onIntInputHandler, onMembershipTypeHandler, onMultipleAnswerChangeHandler)

import Date exposing (Date)
import Engage.Custom.Types exposing (..)
import Engage.Form.MembershipTypeList as MembershipTypeList
import Engage.UI.Datepicker as Datepicker
import Engage.UI.Input exposing (FileInfo)
import Set exposing (Set)


onMembershipTypeHandler : Config msg -> ChangeArgs -> (Maybe MembershipTypeList.MembershipType -> msg)
onMembershipTypeHandler config args =
    MembershipTypeAnswer >> config.onChange args


onFileSelectHandler : Config msg -> ChangeArgs -> (FileInfo -> msg)
onFileSelectHandler config args =
    FileAnswer >> config.onChange args


onChangeHandler : Config msg -> ChangeArgs -> (String -> msg)
onChangeHandler config args =
    (\value -> Answer { value = value }) >> config.onChange args


onMultipleAnswerChangeHandler : Config msg -> ChangeArgs -> (Set String -> msg)
onMultipleAnswerChangeHandler config args =
    (\values -> MultipleAnswer values) >> config.onChange args


onCheckHandler : Config msg -> ChangeArgs -> (Bool -> msg)
onCheckHandler config args =
    let
        boolToAnswer : Bool -> Answer
        boolToAnswer bool =
            BoolAnswer bool

        stateToMsg : Answer -> msg
        stateToMsg =
            config.onChange args
    in
    boolToAnswer >> stateToMsg


onIntInputHandler : Config msg -> ChangeArgs -> (Maybe Int -> msg)
onIntInputHandler config args =
    let
        toMaybeIntMsg maybeAnswerFunc =
            \maybeInt -> maybeAnswerFunc (Answer { value = Maybe.map String.fromInt maybeInt |> Maybe.withDefault "" })
    in
    toMaybeIntMsg (config.onChange args)


onDateChangeHandler : Config msg -> ChangeArgs -> (Datepicker.State -> Maybe Date -> msg)
onDateChangeHandler config args =
    -- Debug.crash "onDateChangeHandler"
    let
        toMaybeDateMsg maybeAnswerFunc dateFormatter =
            \datePickerState maybeDate ->
                maybeAnswerFunc (Answer { value = Maybe.map dateFormatter maybeDate |> Maybe.withDefault "" })
    in
    toMaybeDateMsg (config.onChange args) config.dateFormatter
