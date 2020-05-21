module Engage.UI.ShoppingCart exposing (shoppingCart)

import Css
import Engage.CssHelpers
import Engage.Namespace as Namespace
import Engage.UI.Button as Button
import Html exposing (..)


class =
    Namespace.engagecore
        |> Namespace.toString
        |> Engage.CssHelpers.withNamespace


shoppingCart : Html msg
shoppingCart =
    div []
        [ section [ class [ "ShoppingCart" ] ]
            [ shoppingCartItems
            , total
            , Button.primary
                { attributes = []
                , namespace = Namespace.engagecore
                , text = "Next"
                }
            ]
        ]


shoppingCartItems : Html msg
shoppingCartItems =
    ul []
        [ li [ class [ "ShoppingCartItem" ] ]
            [ span [ class [ "ItemName" ] ] [ text "Abadi Kurniawan" ]
            , span [ class [ "ItemPrice" ] ] [ text "$ 1,500.00" ]
            ]
        , li
            [ class [ "ShoppingCartItem" ] ]
            [ span [ class [ "ItemName" ] ] [ text "Chris Marfia" ]
            , span [ class [ "ItemPrice" ] ] [ text "$ 1,500.00" ]
            ]
        ]


total : Html msg
total =
    div [ class [ "ShoppingCartTotal" ] ] [ span [ class [ "ItemPrice" ] ] [ text "$ 3,000.00" ] ]
