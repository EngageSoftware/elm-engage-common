module Engage.UI.ShoppingCart.Css exposing
    ( Class(..)
    , css
    , snippets
    )

import Css exposing (..)
import Css.Namespace
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Css as BaseCss
import Engage.Theme exposing (Theme)


type Class
    = ShoppingCart
    | ShoppingCartItem
    | ItemName
    | ItemQuantity
    | ItemPrice
    | ShoppingCartTotal


css : Namespace -> Theme -> Stylesheet
css namespace theme =
    (stylesheet << Css.Namespace.namespace (Namespace.toString namespace))
        (snippets theme)


snippets : Theme -> List Snippet
snippets theme =
    [ class ShoppingCart
        [ BaseCss.normalizeMixin
        , border3 (px 1) solid (hex "#aaa")
        ]
    ]
