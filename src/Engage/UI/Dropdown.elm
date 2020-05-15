module Engage.UI.Dropdown exposing
    ( Item
    , State
    , dropdown
    , dropdownWithAttributes
    , initialState
    , reset
    )

import Dropdown
import Engage.Html.Extra as HtmlExtra
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Class(..), Size(..))
import Engage.UI.Dropdown.Css exposing (Class(..))
import Engage.UI.Error as Error exposing (Status(..))
import Engage.UI.Input.Css exposing (Class(Required))
import Engage.UI.Message as Message
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.CssHelpers


type State
    = State Message.State


initialState : State
initialState =
    State Message.initialState


reset : State -> State
reset (State stateData) =
    State Message.initialState


type alias Item =
    { value : String
    , text : String
    , enabled : Bool
    }


dropdown :
    { id : String
    , labelText : String
    , requiredText : Maybe String
    , items : List Item
    , onChange : { onlyStateChange : Bool } -> State -> Maybe String -> msg
    , status : Status
    , namespace : Namespace
    }
    -> State
    -> Maybe String
    -> Html msg
dropdown args state selectedItem =
    dropdownWithAttributes args [] state selectedItem


dropdownWithAttributes :
    { id : String
    , labelText : String
    , requiredText : Maybe String
    , items : List Item
    , onChange : { onlyStateChange : Bool } -> State -> Maybe String -> msg
    , status : Status
    , namespace : Namespace
    }
    -> List (Html.Attribute msg)
    -> State
    -> Maybe String
    -> Html msg
dropdownWithAttributes { id, labelText, requiredText, items, onChange, status, namespace } attributes state selectedItem =
    dropdownWithSizeAndAttributes
        { id = id
        , labelText = labelText
        , requiredText = requiredText
        , items = items
        , onChange = onChange
        , size = Large
        , status = status
        , namespace = namespace
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
    }
    -> List (Html.Attribute msg)
    -> State
    -> Maybe String
    -> Html msg
dropdownWithSizeAndAttributes { id, labelText, requiredText, items, onChange, size, status, namespace } attributes state selectedItem =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        requiredIndicator =
            requiredText
                |> Maybe.map (\required -> span [ class [ Required ], title required ] [ text "*" ])
                |> Maybe.withDefault HtmlExtra.none

        options =
            Dropdown.defaultOptions (onChange { onlyStateChange = False } state)

        stateData =
            unwrap state

        onValidationStateChange errorState =
            onChange { onlyStateChange = True } (State errorState) selectedItem
    in
    div [ class [ FormControl size ] ]
        [ label
            [ class [ Label ]
            , for id
            ]
            [ text labelText, requiredIndicator ]
        , Dropdown.dropdown
            { options | items = items, emptyItem = Just { value = "", text = "", enabled = True } }
            (attributes
                ++ [ Html.Attributes.id id
                   , class [ Dropdown size ]
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
