module Engage.Form.AddressMain exposing (..)

import Engage.Form.Address exposing (..)
import Engage.Namespace as Namespace
import Html exposing (..)


engagecoreNamespace : String
engagecoreNamespace =
    Namespace.toString Namespace.engagecore



--     Html.beginnerProgram
--         { model =
--             ( initialState
--             , { address = ""
--               , unit = ""
--               , country = Nothing
--               , region = Nothing
--               , city = ""
--               , zipCode = ""
--               }
--             )
--         , view = view
--         , update = update
--         }
-- view : ( State, AddressData {} ) -> Html Msg
-- view ( state, model ) =
--     let
--         class =
--             Engage.CssHelpers.withNamespace engagecoreNamespace
--         { css } =
--             Css.compile
--                 [ Engage.Styles.Css.css engagecoreNamespace
--                 , Engage.UI.Input.Css.css engagecoreNamespace Theme.Light
--                 , Engage.UI.Tooltip.Css.css engagecoreNamespace Theme.Light
--                 , Engage.UI.Dropdown.Css.css engagecoreNamespace Theme.Light
--                 , Engage.UI.Button.Css.css engagecoreNamespace Theme.Light
--                 , Engage.UI.Message.Css.css engagecoreNamespace Theme.Light
--                 , Engage.UI.Wizard.Css.css engagecoreNamespace Theme.Light
--                 , Engage.Custom.Form.Css.css Theme.Light
--                 ]
--     in
--     Html.div []
--         [ Html.node "style" [] [ Html.text css ]
--         , Html.map AddressMsg <|
--             Address.form
--                 { localization = Dict.empty
--                 , namespace = "Test"
--                 , countries = Dict.empty
--                 , regions = Dict.empty
--                 }
--                 state
--                 model
--         ]
-- update msg ( state, model ) =
--     case msg of
--         AddressMsg addressMsg ->
--             Address.update addressMsg state model
-- type Msg
--     = AddressMsg Address.Msg
