module Engage.UI.Dialog exposing
    ( Attribute, State
    , cancelText, confirm, hide, initialState, okText, onCancel, onOk, show
    )

{-| UI.Dialog

@docs Attribute, State

@docs cancelText, confirm, hide, initialState, okText, onCancel, onOk, show

-}

import Dom
import Engage.CssHelpers
import Engage.Form.FormAction as FormAction
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.UI.Attribute as Attribute
import Engage.UI.Button as Button
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode as JD
import Task exposing (Task)


domId : String
domId =
    "EngageUIDialog"


{-| The Attribute type
-}
type alias Attribute msg =
    InternalAttribute msg -> InternalAttribute msg


type alias InternalAttribute msg =
    { okText : String
    , cancelText : String
    , onOk : Maybe (State -> msg)
    , onCancel : Maybe (State -> msg)
    }


emptyAttribute : InternalAttribute msg
emptyAttribute =
    { okText = "Ok"
    , cancelText = "Cancel"
    , onOk = Nothing
    , onCancel = Nothing
    }


{-| Get the initial State
-}
initialState : State
initialState =
    State { visible = False }


{-| The State type
-}
type State
    = State { visible : Bool }


{-| Get the confirm view
-}
confirm : Namespace -> State -> List (Attribute msg) -> List (Html msg) -> Html msg
confirm namespace (State state) attributes body =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        attribute =
            Attribute.process emptyAttribute attributes

        visibility =
            if state.visible then
                style []

            else
                style [ ( "display", "none" ) ]

        okClickHandler =
            attribute.onOk
                |> Maybe.map
                    (\msg ->
                        [ onClick (msg (hide (State state)))
                        ]
                    )
                |> Maybe.withDefault []

        cancelClickHandler =
            attribute.onCancel
                |> Maybe.map
                    (\msg ->
                        [ onClick (msg (hide (State state)))
                        ]
                    )
                |> Maybe.withDefault []

        escapeKeyHandler =
            attribute.onCancel
                |> Maybe.map
                    (\msg ->
                        [ on "keydown" (escapeKeyDecoder msg (State state)) ]
                    )
                |> Maybe.withDefault []
    in
    div
        (class [ DialogOverlay ]
            :: visibility
            :: autofocus True
            :: tabindex 0
            :: id domId
            :: escapeKeyHandler
        )
        [ node "dialog"
            [ class [ Dialog ]
            , if state.visible then
                Html.Attributes.attribute "open" "open"

              else
                Html.Attributes.attribute "close" "close"
            ]
            [ div [ class [ DialogQuestion ] ] body
            , FormAction.formAction namespace
                []
                [ div [ class [ DialogAnswer ] ]
                    [ Button.divert
                        { namespace = namespace
                        , attributes =
                            class [ DialogNo ]
                                :: cancelClickHandler
                        , text = attribute.cancelText
                        }
                    , Button.primary
                        { namespace = namespace
                        , attributes =
                            class [ DialogYes ]
                                :: okClickHandler
                        , text = attribute.okText
                        }
                    ]
                ]
            ]
        ]


escapeKeyDecoder : (State -> msg) -> State -> JD.Decoder msg
escapeKeyDecoder msg (State state) =
    Html.Events.keyCode
        |> JD.andThen
            (\keyCode ->
                case keyCode of
                    27 ->
                        JD.succeed (msg (hide (State state)))

                    _ ->
                        JD.fail "We only care for escape key"
            )



-- ATTRIBUTES


{-| Get the onCancel message
-}
onCancel : (State -> msg) -> Attribute msg
onCancel handler =
    \attribute -> { attribute | onCancel = Just handler }


{-| Get the onOk message
-}
onOk : (State -> msg) -> Attribute msg
onOk handler =
    \attribute -> { attribute | onOk = Just handler }


{-| Get the okText message
-}
okText : String -> Attribute msg
okText value =
    \attribute -> { attribute | okText = value }


{-| Get the cancelText message
-}
cancelText : String -> Attribute msg
cancelText value =
    \attribute -> { attribute | cancelText = value }



-- STATE TRANSFORMATION


{-| Show the dialog
-}
show : State -> ( State, Task Dom.Error () )
show (State state) =
    ( State { state | visible = True }
    , Dom.focus domId
    )


{-| Hide the dialog
-}
hide : State -> State
hide (State state) =
    State { state | visible = False }
