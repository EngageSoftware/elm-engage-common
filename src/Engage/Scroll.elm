module Engage.Scroll exposing (scrollTo, scrollToWithConfig)

{-| Engage.Scroll

Scrolling helper functions

@docs scrollTo, scrollToWithConfig

-}

import Browser.Dom
import Ease
import SmoothScroll
import Task


{-| Scroll to a element with the id
-}
scrollTo : String -> msg -> Cmd msg
scrollTo id msg =
    scrollToWithConfig scrollConfig id msg


{-| Scroll to a element with the id with a scroll config
-}
scrollToWithConfig : SmoothScroll.Config -> String -> msg -> Cmd msg
scrollToWithConfig config id msg =
    (Browser.Dom.getElement id
        |> Task.andThen
            (\{ element, viewport, scene } ->
                let
                    yPos =
                        if element.y + viewport.height > scene.height then
                            scene.height - viewport.height

                        else
                            element.y
                in
                SmoothScroll.scrollTo config yPos
            )
    )
        |> Task.attempt (always msg)


scrollConfig : SmoothScroll.Config
scrollConfig =
    SmoothScroll.createConfig Ease.outCubic 200
