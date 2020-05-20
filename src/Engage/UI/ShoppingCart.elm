module Engage.UI.ShoppingCart exposing (shoppingCart)

import Css
import Engage.Styles.Css
import Engage.UI.Button as Button
import Engage.UI.ShoppingCart.Css exposing (Class(..))
import Html exposing (..)


class =
    Engage.Styles.Css.cssHelpers


shoppingCart : Html msg
shoppingCart =
    let
        { css } =
            Css.compile [ Engage.UI.ShoppingCart.Css.css ]
    in
    div []
        [ node "style" [] [ text css ]
        , section [ class [ ShoppingCart ] ]
            [ shoppingCartItems
            , total
            , Button.primary
                { attributes = []
                , text = "Next"
                }
            ]
        ]


shoppingCartItems : Html msg
shoppingCartItems =
    ul []
        [ li [ class [ ShoppingCartItem ] ]
            [ span [ class [ ItemName ] ] [ text "Abadi Kurniawan" ]
            , span [ class [ ItemPrice ] ] [ text "$ 1,500.00" ]
            ]
        , li
            [ class [ ShoppingCartItem ] ]
            [ span [ class [ ItemName ] ] [ text "Chris Marfia" ]
            , span [ class [ ItemPrice ] ] [ text "$ 1,500.00" ]
            ]
        ]


total : Html msg
total =
    div [ class [ ShoppingCartTotal ] ] [ span [ class [ ItemPrice ] ] [ text "$ 3,000.00" ] ]
