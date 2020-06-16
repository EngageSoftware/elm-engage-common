module Engage.UI.Dropdown exposing
    ( Item, State
    , dropdown, dropdownWithAttributes, initialState, reset
    )

{-| UI.Dropdown

@docs Item, State

@docs dropdown, dropdownWithAttributes, initialState, reset

-}

import Dropdown
import Engage.CssHelpers
import Engage.Html.Extra as HtmlExtra
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Class(..), Size(..), getSizeString)
import Engage.UI.Error as Error exposing (Status(..))
import Engage.UI.Message as Message
import Html exposing (..)
import Html.Attributes exposing (..)


{-| The State type
-}
type State
    = State Message.State


{-| Get the initial State
-}
initialState : State
initialState =
    State Message.initialState


{-| Reset the State
-}
reset : State -> State
reset (State stateData) =
    State Message.initialState


{-| The Item type
-}
type alias Item =
    { value : String
    , text : String
    , enabled : Bool
    }


{-| Get the dropdown view
-}
dropdown :
    { id : String
    , labelText : String
    , requiredText : Maybe String
    , items : List Item
    , onChange : { onlyStateChange : Bool } -> State -> Maybe String -> msg
    , status : Status
    , namespace : Namespace
    , withEmptyItem : Bool
    }
    -> State
    -> Maybe String
    -> Html msg
dropdown args state selectedItem =
    dropdownWithAttributes args [] state selectedItem


{-| Get the dropdown with attributes view
-}
dropdownWithAttributes :
    { id : String
    , labelText : String
    , requiredText : Maybe String
    , items : List Item
    , onChange : { onlyStateChange : Bool } -> State -> Maybe String -> msg
    , status : Status
    , namespace : Namespace
    , withEmptyItem : Bool
    }
    -> List (Html.Attribute msg)
    -> State
    -> Maybe String
    -> Html msg
dropdownWithAttributes { id, labelText, requiredText, items, onChange, status, namespace, withEmptyItem } attributes state selectedItem =
    dropdownWithSizeAndAttributes
        { id = id
        , labelText = labelText
        , requiredText = requiredText
        , items = items
        , onChange = onChange
        , size = Large
        , status = status
        , namespace = namespace
        , withEmptyItem = withEmptyItem
        }
        attributes
        state
        selectedItem


dropdownWithSizeAndAttributes :
    { id : String
    , labelText : String
    , requiredText : Maybe String
    , items : List Item
    , onChange : { onlyStateChange : Bool } -> State -> Maybe String -> msg
    , size : Size
    , namespace : Namespace
    , status : Status
    , withEmptyItem : Bool
    }
    -> List (Html.Attribute msg)
    -> State
    -> Maybe String
    -> Html msg
dropdownWithSizeAndAttributes { id, labelText, requiredText, items, onChange, size, status, namespace, withEmptyItem } attributes state selectedItem =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        requiredIndicator =
            requiredText
                |> Maybe.map (\required -> span [ class [ "Required" ], title required ] [ text "*" ])
                |> Maybe.withDefault HtmlExtra.none

        options =
            Dropdown.defaultOptions (onChange { onlyStateChange = False } state)

        stateData =
            unwrap state

        onValidationStateChange errorState =
            onChange { onlyStateChange = True } (State errorState) selectedItem
    in
    div [ class [ "FormControl-" ++ getSizeString size ] ]
        [ label
            [ class [ "Label" ]
            , for id
            ]
            [ text labelText, requiredIndicator ]
        , Dropdown.dropdown
            { options
                | items = items
                , emptyItem =
                    if withEmptyItem then
                        Just { value = "", text = "", enabled = True }

                    else
                        Nothing
            }
            (attributes
                ++ [ Html.Attributes.id id
                   , class [ "Dropdown-" ++ getSizeString size ]
                   ]
            )
            selectedItem
        , Error.inlineError
            { namespace = namespace
            , status = status
            , onChange = onValidationStateChange
            }
            stateData
        ]


unwrap : State -> Message.State
unwrap (State stateData) =
    stateData
