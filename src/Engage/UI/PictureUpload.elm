module Engage.UI.PictureUpload exposing
    ( Attribute, File, PortOutKey(..)
    , browse, dropZone, onFiles, onLoad, picture, pictureUpload, remove
    )

{-| UI.PictureUpload

@docs Attribute, File, PortOutKey

@docs browse, dropZone, onFiles, onLoad, picture, pictureUpload, remove

-}

import Engage.CssHelpers
import Engage.Html.Extra as HtmlExtra
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.UI.Attribute as Attribute
import Engage.UI.Button as Button
import Engage.UI.Svg as Svg
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed
import Json.Decode
import Svg.Attributes
import Time exposing (Posix)


{-| The PortOutKey type
-}
type PortOutKey
    = PictureUploadLoaded


{-| The File type
-}
type alias File =
    { lastModified : Maybe Posix
    , name : String
    , size : Int
    , mimeType : String
    , dataURL : String
    }


fileDecoder : Json.Decode.Decoder File
fileDecoder =
    Json.Decode.map5 File
        (Json.Decode.maybe (Json.Decode.field "lastModified" Json.Decode.int |> Json.Decode.map Time.millisToPosix))
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "size" Json.Decode.int)
        (Json.Decode.field "mimeType" Json.Decode.string)
        (Json.Decode.field "dataURL" Json.Decode.string)


type alias InternalAttribute msg =
    { browseButton :
        Maybe
            { text : String
            , msg : msg
            }
    , dropZoneText : String
    , onLoad : Maybe (String -> msg)
    , onFiles : Maybe (List File -> msg)
    , pictureData : String
    , onRemove : Maybe ( String, msg )
    }


emptyAttribute : InternalAttribute msg
emptyAttribute =
    { browseButton = Nothing
    , dropZoneText = "Drop your file here"
    , onLoad = Nothing
    , onFiles = Nothing
    , pictureData = ""
    , onRemove = Nothing
    }


{-| The Attribute type
-}
type alias Attribute msg =
    InternalAttribute msg -> InternalAttribute msg


{-| Get the browse Attribute
-}
browse : String -> msg -> Attribute msg
browse text msg =
    \attribute -> { attribute | browseButton = Just { text = text, msg = msg } }


{-| Get the drop zone Attribute
-}
dropZone : String -> Attribute msg
dropZone text =
    \attribute -> { attribute | dropZoneText = text }


{-| Get the onLoad Attribute
-}
onLoad : (String -> msg) -> Attribute msg
onLoad msg =
    \attribute -> { attribute | onLoad = Just msg }


{-| Get the onFiles Attribute
-}
onFiles : (List File -> msg) -> Attribute msg
onFiles msg =
    \attribute -> { attribute | onFiles = Just msg }


{-| Get the picture Attribute
-}
picture : String -> Attribute msg
picture pictureData =
    \attribute -> { attribute | pictureData = pictureData }


{-| Get the remove Attribute
-}
remove : String -> msg -> Attribute msg
remove text msg =
    \attribute -> { attribute | onRemove = Just ( text, msg ) }


{-| Get the picture upload view
-}
pictureUpload : Namespace -> String -> List (Attribute msg) -> Html msg
pictureUpload namespace domId attributes =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        attribute =
            Attribute.process emptyAttribute attributes
    in
    Html.Keyed.node "div"
        [ class [ "PictureUpload" ] ]
        [ ( domId, dropZoneView namespace attribute domId )
        , ( "removeButton", removeButton namespace attribute )

        --, ( "browseButton", browseButton namespace attribute )
        ]


{-| Get the drop zone view
-}
dropZoneView : Namespace -> InternalAttribute msg -> String -> Html msg
dropZoneView namespace attribute domId =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        onFilesUpload : (List File -> msg) -> Html.Attribute msg
        onFilesUpload msg =
            Html.Events.on "files"
                (Json.Decode.map msg
                    (Json.Decode.field "detail"
                        (Json.Decode.list fileDecoder)
                    )
                )
    in
    div
        ([ class [ "PictureUploadDropZone" ]
         , id domId
         , if String.isEmpty attribute.pictureData then
            style "" ""

           else
            style "background-image" ("url(" ++ attribute.pictureData ++ ")")
         ]
            ++ (attribute.onFiles
                    |> Maybe.map (onFilesUpload >> List.singleton)
                    |> Maybe.withDefault []
               )
        )
        [ div [ class [ "PictureUploadButton" ] ]
            [ Html.input [ Html.Attributes.type_ "file", accept "image/*", multiple True, class [ "PictureUploadFileInput" ] ] []
            , if String.isEmpty attribute.pictureData then
                Svg.upload namespace "Upload" [ width 50, height 50 ]

              else
                text ""
            ]
        , if String.isEmpty attribute.pictureData then
            text <| attribute.dropZoneText

          else
            text ""
        , attribute.onLoad
            |> Maybe.map (\msg -> HtmlExtra.domLoadNotifier (msg domId))
            |> Maybe.withDefault HtmlExtra.none
        ]


removeButton : Namespace -> InternalAttribute msg -> Html msg
removeButton namespace attribute =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    if String.isEmpty attribute.pictureData then
        text ""

    else
        attribute.onRemove
            |> Maybe.map
                (\( text, msg ) ->
                    button [ type_ "button", class [ "PictureUploadRemoveButton" ], onClick msg, title text ]
                        [ Svg.remove namespace text [ Svg.Attributes.width "100%", Svg.Attributes.height "100%" ] ]
                )
            |> Maybe.withDefault (text "")


pictureView : Namespace -> String -> Html msg
pictureView namespace pictureData =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    if String.isEmpty pictureData then
        HtmlExtra.none

    else
        img [ class [ "PictureUploadPreview" ], src pictureData ] []


browseButton : Namespace -> InternalAttribute msg -> Html msg
browseButton namespace attribute =
    attribute.browseButton
        |> Maybe.map (\{ text, msg } -> Button.standardSmall { attributes = [ onClick msg ], text = text, namespace = namespace })
        |> Maybe.withDefault HtmlExtra.none
