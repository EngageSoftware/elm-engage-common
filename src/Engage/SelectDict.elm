module Engage.SelectDict exposing
    ( SelectDict
    , after
    , before
    , fromDicts
    , insertAfter
    , insertBefore
    , keys
    , map
    , next
    , prev
    , segments
    , select
    , selected
    , selectedKey
    , selectedValue
    , size
    , toDict
    , toSelectList
    , updateSelected
    , values
    )

{-| SelectDict

@docs SelectDict

@docs after, before, fromDicts, insertAfter, insertBefore, keys, map, next, prev, segments, select, selected, selectedKey, selectedValue, size, toDict, toSelectList, updateSelected, values

-}

import Dict exposing (Dict)
import SelectList exposing (SelectList)


{-| A SelectDict type
-}
type SelectDict comparable a
    = SelectDict { before : Dict comparable a, selected : ( comparable, a ), after : Dict comparable a }


{-| Get the next SelectDict
-}
next : SelectDict comparable a -> SelectDict comparable a
next original =
    original
        |> after
        |> Dict.toList
        |> List.head
        |> Maybe.map (nextHelper original)
        |> Maybe.withDefault original


nextHelper : SelectDict comparable a -> ( comparable, a ) -> SelectDict comparable a
nextHelper original nextSelected =
    SelectDict
        { before = Dict.insert (selectedKey original) (selectedValue original) (before original)
        , selected = nextSelected
        , after = Dict.remove (Tuple.first nextSelected) (after original)
        }


{-| Get the previous SelectDict
-}
prev : SelectDict comparable a -> SelectDict comparable a
prev original =
    original
        |> before
        |> Dict.toList
        |> List.reverse
        |> List.head
        |> Maybe.map (prevHelper original)
        |> Maybe.withDefault original


prevHelper : SelectDict comparable a -> ( comparable, a ) -> SelectDict comparable a
prevHelper original nextSelected =
    SelectDict
        { before = Dict.remove (Tuple.first nextSelected) (before original)
        , selected = nextSelected
        , after = Dict.insert (selectedKey original) (selectedValue original) (after original)
        }


{-| Update the selected SelectDict
-}
updateSelected : (comparable -> a -> a) -> SelectDict comparable a -> SelectDict comparable a
updateSelected updater selectDict =
    let
        ( before, ( id, selected ), after ) =
            segments selectDict
    in
    fromDicts before ( id, updater id selected ) after


{-| Convert a normal Dict into a SelectDict
-}
fromDicts : Dict comparable a -> ( comparable, a ) -> Dict comparable a -> SelectDict comparable a
fromDicts before selected after =
    SelectDict { before = before, selected = selected, after = after }


{-| Get a normal Dict from a SelectDict
-}
toDict : SelectDict comparable a -> Dict comparable a
toDict selectDict =
    case selectDict of
        SelectDict { before, selected, after } ->
            Dict.union before after
                |> Dict.insert (Tuple.first selected) (Tuple.second selected)


{-| Get the before Dict
-}
before : SelectDict comparable a -> Dict comparable a
before (SelectDict { before }) =
    before


{-| Get the after Dict
-}
after : SelectDict comparable a -> Dict comparable a
after (SelectDict { after }) =
    after


{-| Get the selected Dict
-}
selected : SelectDict comparable a -> ( comparable, a )
selected (SelectDict { selected }) =
    selected


{-| Get the selected value
-}
selectedValue : SelectDict comparable a -> a
selectedValue (SelectDict { selected }) =
    selected |> Tuple.second


{-| Get the selected key
-}
selectedKey : SelectDict comparable a -> comparable
selectedKey (SelectDict { selected }) =
    selected |> Tuple.first


{-| Get the segments of the SelectDict
-}
segments : SelectDict comparable a -> ( Dict comparable a, ( comparable, a ), Dict comparable a )
segments selectDict =
    case selectDict of
        SelectDict { before, selected, after } ->
            ( before, selected, after )


{-| Get a SelectList from a SelectDict
-}
toSelectList : SelectDict comparable a -> SelectList a
toSelectList selectDict =
    let
        ( before, ( _, selected ), after ) =
            segments selectDict
    in
    SelectList.fromLists (Dict.values before) selected (Dict.values after)


{-| Select something from the SelectDict
-}
select : comparable -> SelectDict comparable a -> SelectDict comparable a
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


selectFromBefore : ( comparable, a ) -> SelectDict comparable a -> SelectDict comparable a
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


selectFromAfter : ( comparable, a ) -> SelectDict comparable a -> SelectDict comparable a
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
map : (comparable -> a -> b) -> SelectDict comparable a -> SelectDict comparable b
map func ((SelectDict { before, selected, after }) as origin) =
    fromDicts (Dict.map func before)
        ( Tuple.first selected, uncurry func selected )
        (Dict.map func after)


{-| Get the keys of the SelectDict
-}
keys : SelectDict comparable a -> List comparable
keys =
    toDict >> Dict.keys


{-| Get the values of the SelectDict
-}
values : SelectDict comparable a -> List a
values =
    toDict >> Dict.values


{-| Insert into the after Dict
-}
insertAfter : comparable -> a -> SelectDict comparable a -> SelectDict comparable a
insertAfter key value ((SelectDict { before, selected, after }) as origin) =
    fromDicts before selected (Dict.insert key value after)


{-| Insert into the before Dict
-}
insertBefore : comparable -> a -> SelectDict comparable a -> SelectDict comparable a
insertBefore key value ((SelectDict { before, selected, after }) as origin) =
    fromDicts (Dict.insert key value after) selected after


{-| Get the size of the SelectDict
-}
size : SelectDict comparable a -> Int
size (SelectDict { before, selected, after }) =
    Dict.size before + Dict.size after + 1