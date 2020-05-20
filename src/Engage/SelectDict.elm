module Engage.SelectDict exposing
    ( SelectDict
    , getAfter, getBefore, fromDicts, insertAfter, insertBefore, keys, map, next, prev, segments, select, getSelected, selectedKey, selectedValue, size, toDict, toSelectList, updateSelected, values
    )

{-| SelectDict

@docs SelectDict

@docs getAfter, getBefore, fromDicts, insertAfter, insertBefore, keys, map, next, prev, segments, select, getSelected, selectedKey, selectedValue, size, toDict, toSelectList, updateSelected, values

-}

import Dict exposing (Dict)
import SelectList exposing (SelectList)


{-| A SelectDict type
-}
type SelectDict a
    = SelectDict { before : Dict Int a, selected : ( Int, a ), after : Dict Int a }


{-| Get the next SelectDict
-}
next : SelectDict a -> SelectDict a
next original =
    original
        |> getAfter
        |> Dict.toList
        |> List.head
        |> Maybe.map (nextHelper original)
        |> Maybe.withDefault original


nextHelper : SelectDict a -> ( Int, a ) -> SelectDict a
nextHelper original nextSelected =
    SelectDict
        { before = Dict.insert (selectedKey original) (selectedValue original) (getBefore original)
        , selected = nextSelected
        , after = Dict.remove (Tuple.first nextSelected) (getAfter original)
        }


{-| Get the previous SelectDict
-}
prev : SelectDict a -> SelectDict a
prev original =
    original
        |> getBefore
        |> Dict.toList
        |> List.reverse
        |> List.head
        |> Maybe.map (prevHelper original)
        |> Maybe.withDefault original


prevHelper : SelectDict a -> ( Int, a ) -> SelectDict a
prevHelper original nextSelected =
    SelectDict
        { before = Dict.remove (Tuple.first nextSelected) (getBefore original)
        , selected = nextSelected
        , after = Dict.insert (selectedKey original) (selectedValue original) (getAfter original)
        }


{-| Update the selected SelectDict
-}
updateSelected : (Int -> a -> a) -> SelectDict a -> SelectDict a
updateSelected updater selectDict =
    let
        ( before, ( id, selected ), after ) =
            segments selectDict
    in
    fromDicts before ( id, updater id selected ) after


{-| Convert a normal Dict into a SelectDict
-}
fromDicts : Dict Int a -> ( Int, a ) -> Dict Int a -> SelectDict a
fromDicts before selected after =
    SelectDict { before = before, selected = selected, after = after }


{-| Get a normal Dict from a SelectDict
-}
toDict : SelectDict a -> Dict Int a
toDict selectDict =
    case selectDict of
        SelectDict { before, selected, after } ->
            Dict.union before after
                |> Dict.insert (Tuple.first selected) (Tuple.second selected)


{-| Get the before Dict
-}
getBefore : SelectDict a -> Dict Int a
getBefore (SelectDict { before }) =
    before


{-| Get the after Dict
-}
getAfter : SelectDict a -> Dict Int a
getAfter (SelectDict { after }) =
    after


{-| Get the selected Dict
-}
getSelected : SelectDict a -> ( Int, a )
getSelected (SelectDict { selected }) =
    selected


{-| Get the selected value
-}
selectedValue : SelectDict a -> a
selectedValue (SelectDict { selected }) =
    selected |> Tuple.second


{-| Get the selected key
-}
selectedKey : SelectDict a -> Int
selectedKey (SelectDict { selected }) =
    selected |> Tuple.first


{-| Get the segments of the SelectDict
-}
segments : SelectDict a -> ( Dict Int a, ( Int, a ), Dict Int a )
segments selectDict =
    case selectDict of
        SelectDict { before, selected, after } ->
            ( before, selected, after )


{-| Get a SelectList from a SelectDict
-}
toSelectList : SelectDict a -> SelectList a
toSelectList selectDict =
    let
        ( before, ( _, selected ), after ) =
            segments selectDict
    in
    SelectList.fromLists (Dict.values before) selected (Dict.values after)


{-| Select something from the SelectDict
-}
select : Int -> SelectDict a -> SelectDict a
select id ((SelectDict { before, selected, after }) as original) =
    case Dict.get id before of
        Just newSelectedValue ->
            selectFromBefore ( id, newSelectedValue ) original

        Nothing ->
            case Dict.get id after of
                Just newSelectedValue ->
                    selectFromAfter ( id, newSelectedValue ) original

                Nothing ->
                    original


selectFromBefore : ( Int, a ) -> SelectDict a -> SelectDict a
selectFromBefore (( id, value ) as newSelected) ((SelectDict { before, selected, after }) as original) =
    let
        originalId =
            Tuple.first selected

        originalValue =
            Tuple.second selected

        newBefore =
            Dict.filter (\i v -> i < id) before

        additionalAfter =
            Dict.filter (\i v -> i > id) before
    in
    fromDicts newBefore newSelected (Dict.union additionalAfter (Dict.insert originalId originalValue after))


selectFromAfter : ( Int, a ) -> SelectDict a -> SelectDict a
selectFromAfter (( id, value ) as newSelected) ((SelectDict { before, selected, after }) as original) =
    let
        originalId =
            Tuple.first selected

        originalValue =
            Tuple.second selected

        newAfter =
            Dict.filter (\i v -> i > id) after

        additionalBefore =
            Dict.filter (\i v -> i < id) after
    in
    fromDicts (Dict.union additionalBefore (Dict.insert originalId originalValue before)) newSelected newAfter


{-| Map a SelectDict
-}
map : (Int -> a -> b) -> SelectDict a -> SelectDict b
map func ((SelectDict { before, selected, after }) as origin) =
    fromDicts (Dict.map func before)
        ( Tuple.first selected, uncurry func selected )
        (Dict.map func after)


{-| Get the keys of the SelectDict
-}
keys : SelectDict a -> List Int
keys =
    toDict >> Dict.keys


{-| Get the values of the SelectDict
-}
values : SelectDict a -> List a
values =
    toDict >> Dict.values


{-| Insert into the after Dict
-}
insertAfter : Int -> a -> SelectDict a -> SelectDict a
insertAfter key value ((SelectDict { before, selected, after }) as origin) =
    fromDicts before selected (Dict.insert key value after)


{-| Insert into the before Dict
-}
insertBefore : Int -> a -> SelectDict a -> SelectDict a
insertBefore key value ((SelectDict { before, selected, after }) as origin) =
    fromDicts (Dict.insert key value after) selected after


{-| Get the size of the SelectDict
-}
size : SelectDict a -> Int
size (SelectDict { before, selected, after }) =
    Dict.size before + Dict.size after + 1


{-| Change how arguments are passed to a function.
This combines two arguments into a single pair.
-}
uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b
