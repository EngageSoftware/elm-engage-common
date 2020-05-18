module Engage.ListData exposing
    ( Edit(..)
    , ListData(..)
    , cancelEdit
    , decoder
    , dict
    , editView
    , empty
    , fromList
    , getEdit
    , isEditing
    , isEmpty
    , mapEdit
    , newData
    , setEdit
    , setState
    , view
    )

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as JD exposing (Decoder)


type ListData state a comparable
    = ListData (Dict comparable a) (Edit state a)


type Edit state a
    = NoEdit
    | Edit state a


dict : ListData state a comparable -> Dict comparable a
dict (ListData dict _) =
    dict


isEmpty : ListData state a comparable -> Bool
isEmpty (ListData dict _) =
    Dict.isEmpty dict


empty : ListData state a comparable
empty =
    ListData Dict.empty NoEdit


decoder : (a -> comparable) -> Decoder (List a) -> Decoder (ListData state a comparable)
decoder toKey listDecoder =
    listDecoder
        |> JD.map (fromList toKey)


fromList : (a -> comparable) -> List a -> ListData state a comparable
fromList toKey list =
    let
        dict =
            list
                |> List.map (\a -> ( toKey a, a ))
                |> Dict.fromList
    in
    ListData dict NoEdit


view : (a -> comparable) -> (a -> comparable) -> (a -> Html msg) -> ListData state a comparable -> List (Html msg)
view sorter toKey viewer (ListData dict edit) =
    let
        isEditing =
            case edit of
                NoEdit ->
                    False

                Edit _ _ ->
                    True

        editedKey =
            case edit of
                NoEdit ->
                    Nothing

                Edit _ a ->
                    Dict.get (toKey a) dict
                        |> Maybe.map toKey

        list =
            editedKey
                |> Maybe.map (\key -> Dict.remove key dict)
                |> Maybe.withDefault dict
                |> Dict.values
                |> List.sortBy sorter
    in
    list
        |> List.map viewer


newData : state -> a -> ListData state a comparable -> ListData state a comparable
newData state a (ListData dict edit) =
    ListData dict (Edit state a)


setEdit : state -> comparable -> ListData state a comparable -> ListData state a comparable
setEdit state key (ListData dict edit) =
    case Dict.get key dict of
        Just a ->
            ListData dict (Edit state a)

        Nothing ->
            ListData dict edit


getEdit : ListData state a comparable -> Maybe ( state, a )
getEdit (ListData dict edit) =
    case edit of
        NoEdit ->
            Nothing

        Edit state a ->
            Just ( state, a )


isEditing : ListData state a comparable -> Bool
isEditing (ListData dict edit) =
    case edit of
        NoEdit ->
            False

        Edit state a ->
            True


editView : (state -> a -> Html msg) -> ListData state a comparable -> Html msg
editView viewer (ListData dict edit) =
    case edit of
        NoEdit ->
            Html.text ""

        Edit state a ->
            viewer state a


mapEdit : (state -> a -> ( state, a )) -> ListData state a comparable -> ListData state a comparable
mapEdit update (ListData dict edit) =
    case edit of
        NoEdit ->
            ListData dict edit

        Edit state a ->
            let
                ( newState, newA ) =
                    update state a
            in
            ListData dict (Edit newState newA)


cancelEdit : ListData state a comparable -> ListData state a comparable
cancelEdit (ListData dict edit) =
    ListData dict NoEdit


setState : state -> ListData state a comparable -> ListData state a comparable
setState state (ListData dict edit) =
    case edit of
        NoEdit ->
            ListData dict edit

        Edit _ a ->
            ListData dict (Edit state a)
