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

{-| ListData

@docs ListData, Edit

@docs cancelEdit, decoder, dict, editView, empty, fromList, getEdit, isEditing, isEmpty, mapEdit, newData, setEdit, setState, view

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as JD exposing (Decoder)


{-| A ListData type
-}
type ListData state a comparable
    = ListData (Dict comparable a) (Edit state a)


{-| An Edit type
-}
type Edit state a
    = NoEdit
    | Edit state a


{-| Get the dict from the list data
-}
dict : ListData state a comparable -> Dict comparable a
dict (ListData dict _) =
    dict


{-| Check if the list is empty
-}
isEmpty : ListData state a comparable -> Bool
isEmpty (ListData dict _) =
    Dict.isEmpty dict


{-| Get an empty ListData
-}
empty : ListData state a comparable
empty =
    ListData Dict.empty NoEdit


{-| ListData decoder
-}
decoder : (a -> comparable) -> Decoder (List a) -> Decoder (ListData state a comparable)
decoder toKey listDecoder =
    listDecoder
        |> JD.map (fromList toKey)


{-| Get a ListData from a List
-}
fromList : (a -> comparable) -> List a -> ListData state a comparable
fromList toKey list =
    let
        dict =
            list
                |> List.map (\a -> ( toKey a, a ))
                |> Dict.fromList
    in
    ListData dict NoEdit


{-| View helper for a ListData
-}
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


{-| Get a new list data from new data
-}
newData : state -> a -> ListData state a comparable -> ListData state a comparable
newData state a (ListData dict edit) =
    ListData dict (Edit state a)


{-| Set the edit of the ListData
-}
setEdit : state -> comparable -> ListData state a comparable -> ListData state a comparable
setEdit state key (ListData dict edit) =
    case Dict.get key dict of
        Just a ->
            ListData dict (Edit state a)

        Nothing ->
            ListData dict edit


{-| Get the edit of the ListData
-}
getEdit : ListData state a comparable -> Maybe ( state, a )
getEdit (ListData dict edit) =
    case edit of
        NoEdit ->
            Nothing

        Edit state a ->
            Just ( state, a )


{-| Check if the ListData is editing
-}
isEditing : ListData state a comparable -> Bool
isEditing (ListData dict edit) =
    case edit of
        NoEdit ->
            False

        Edit state a ->
            True


{-| Edit view helper
-}
editView : (state -> a -> Html msg) -> ListData state a comparable -> Html msg
editView viewer (ListData dict edit) =
    case edit of
        NoEdit ->
            Html.text ""

        Edit state a ->
            viewer state a


{-| Map an edit to a ListData
-}
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


{-| Cancel an edit
-}
cancelEdit : ListData state a comparable -> ListData state a comparable
cancelEdit (ListData dict edit) =
    ListData dict NoEdit


{-| Set the editing state
-}
setState : state -> ListData state a comparable -> ListData state a comparable
setState state (ListData dict edit) =
    case edit of
        NoEdit ->
            ListData dict edit

        Edit _ a ->
            ListData dict (Edit state a)
