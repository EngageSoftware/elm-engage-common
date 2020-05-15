module Engage.UI.PictureUpload.Css exposing (Class(..), css)

import Css exposing (..)
import Css.Namespace
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Css as BaseCss
import Engage.Theme as Theme exposing (Theme)


type Class
    = PictureUpload
    | PictureUploadDropZone
    | PictureUploadPreview
    | PictureUploadBrowseButton
    | PictureUploadRemoveButton


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ pictureUploadSnippet
    , dropZoneSnippet
    , browseButtonSnippet
    , previewSnippet
    , removeButtonSnippet
    ]


removeButtonSnippet : Snippet
removeButtonSnippet =
    class PictureUploadRemoveButton
        [ BaseCss.normalizeMixin
        , position absolute
        , top zero
        , right zero
        , backgroundColor (rgba 255 255 255 0.5)
        , padding zero
        , margin zero
        , border zero
        , width (px 24)
        , height (px 24)
        ]


pictureUploadSnippet : Snippet
pictureUploadSnippet =
    class PictureUpload
        [ BaseCss.normalizeMixin
        , position relative
        , displayFlex
        , flexDirection column
        , justifyContent flexEnd
        , border3 (px 3) solid (rgba 0 0 0 0.25)
        ]


dropZoneSnippet : Snippet
dropZoneSnippet =
    class PictureUploadDropZone
        [ BaseCss.normalizeMixin
        , height (px 300)
        , backgroundColor (rgba 0 0 0 0.125)
        , displayFlex
        , flexDirection column
        , justifyContent center
        , alignItems center
        , backgroundSize contain
        , backgroundPosition center
        , backgroundRepeat noRepeat
        ]


browseButtonSnippet : Snippet
browseButtonSnippet =
    class PictureUploadBrowseButton
        [ BaseCss.normalizeMixin
        ]


previewSnippet : Snippet
previewSnippet =
    class PictureUploadPreview
        [ BaseCss.normalizeMixin
        , maxWidth (pct 100)
        , maxHeight (pct 100)
        ]
