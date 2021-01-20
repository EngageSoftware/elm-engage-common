module Engage.UI.Wizard exposing
    ( Config, Page, ShoppingCart, SinglePageType(..), State, Step
    , defaultConfig, getStepError, getStepModel, getStepTitle, initialState, multiPages, singlePage, wizard
    )

{-| UI.Wizard

@docs Config, Page, ShoppingCart, SinglePageType, State, Step

@docs defaultConfig, getStepError, getStepModel, getStepTitle, initialState, multiPages, singlePage, wizard

-}

import Dict
import Engage.CssHelpers
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.SelectDict as SelectDict exposing (SelectDict)
import Engage.UI.Button as Button
import Engage.UI.Error as Error exposing (Status)
import Engage.UI.Svg as Svg
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Extra
import Json.Decode
import Maybe.Extra
import Mustache



-- TYPES


{-| Wizard Step data
-}
type Step model
    = Single { singlePageType : SinglePageType, title : String, status : Status, model : model }
    | Multi { title : String, pages : SelectDict Int (Page model) }


{-| A SinglePageType type
-}
type SinglePageType
    = SinglePageInfo
    | SinglePageForm


{-| Get a single page Step
-}
singlePage : { singlePageType : SinglePageType, title : String, status : Status, model : model } -> Step model
singlePage =
    Single


{-| Get a multi pages Step
-}
multiPages : { title : String, pages : SelectDict Int (Page model) } -> Step model
multiPages =
    Multi


{-| A Page type
-}
type alias Page model =
    { title : String
    , status : Status
    , model : model
    }


{-| A ShoppingCart type
-}
type alias ShoppingCart msg =
    { content : Html msg }


{-| Configuration for the Wizard.

  - pageIndicatorTemplate: uses mustache templates. Default: "Page {{ current }} of {{ total }}"
  - onState: callback function for when internal state of the wizard changes.

-}
type alias Config msg model =
    { pageIndicatorTemplate : String
    , nextButtonTemplate : String
    , prevButtonTemplate : String
    , finishButtonTemplate : String
    , reviewButtonTemplate : String
    , title : String
    , onNextStep : State -> msg
    , onGotoStep : { stepId : Int } -> State -> msg
    , onNextPage : State -> msg
    , onState : State -> msg
    , onFinish : State -> msg
    , onReview : State -> msg
    , stepRenderer : model -> List (Html msg)
    , currentPageRenderer : model -> List (Html msg)
    , beforePageRenderer : model -> List (Html msg)
    , afterPageRenderer : model -> List (Html msg)
    , showReview : Bool
    }


{-| Get the default config
-}
defaultConfig :
    { onState : State -> msg
    , onNextStep : State -> msg
    , onGotoStep : { stepId : Int } -> State -> msg
    , onNextPage : State -> msg
    , onFinish : State -> msg
    , onReview : State -> msg
    }
    -> Config msg model
defaultConfig { onState, onNextStep, onGotoStep, onNextPage, onFinish, onReview } =
    { pageIndicatorTemplate = "Page {{ currentPage }} of {{ totalPage }}"
    , nextButtonTemplate = "Next - {{ nextTitle }}"
    , prevButtonTemplate = "Prev - {{ prevTitle }}"
    , finishButtonTemplate = "Finish"
    , reviewButtonTemplate = "Review"
    , title = ""
    , onState = onState
    , onNextStep = onNextStep
    , onNextPage = onNextPage
    , onGotoStep = onGotoStep
    , onReview = onReview
    , onFinish = onFinish
    , stepRenderer = always [ Html.text "NOT IMPLEMENTED" ]
    , currentPageRenderer = always [ Html.text "NOT IMPLEMENTED" ]
    , beforePageRenderer = always [ Html.text "NOT IMPLEMENTED" ]
    , afterPageRenderer = always [ Html.text "NOT IMPLEMENTED" ]
    , showReview = True
    }


{-| A State type
-}
type State
    = State StateData


type alias StateData =
    { navigationStatus : NavigationStatus
    , inReview : Bool
    }


type NavigationStatus
    = ExpandedState
    | CollapsedState



-- VIEWS


{-| Get the wizard view
-}
wizard :
    { namespace : Namespace
    , config : Config msg model
    , state : State
    , shoppingCart : ShoppingCart msg
    , isLoading : Bool
    , localize : String -> String
    }
    -> SelectDict Int (Step model)
    -> Html msg
wizard ({ namespace, state, shoppingCart } as args) steps =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    section [ class [ "Wizard" ] ]
        [ wizardHeader args state steps
        , viewStep args state steps
        , errorMessage args steps
        , navigationControl args state steps
        , shoppingCart.content
        ]


viewStep : { a | namespace : Namespace, config : Config msg model } -> State -> SelectDict Int (Step model) -> Html msg
viewStep { namespace, config } state steps =
    let
        step =
            SelectDict.getSelected steps

        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    case step of
        ( _, Single { model } ) ->
            div [ class [ "WizardBody" ] ] (config.stepRenderer model)

        ( stepId, Multi { pages } ) ->
            viewMultiPages
                { config = config
                , state = state
                , stepId = stepId
                , namespace = namespace
                }
                pages


viewMultiPages : { namespace : Namespace, config : Config msg model, state : State, stepId : Int } -> SelectDict Int (Page model) -> Html msg
viewMultiPages { namespace, config, state } pages =
    let
        ( before, selected, after ) =
            SelectDict.segments pages

        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        stateData =
            unwrap state

        currentPageRenderer =
            if stateData.inReview then
                config.beforePageRenderer

            else
                config.currentPageRenderer
    in
    div [ class [ "WizardBody" ] ]
        ((before
            |> Dict.map (renderBeforePage config state)
            |> Dict.values
            |> List.foldr List.append []
         )
            ++ (selected
                    |> Tuple.second
                    |> (.model >> currentPageRenderer)
               )
            ++ (after
                    |> Dict.values
                    |> List.concatMap (.model >> config.afterPageRenderer)
               )
        )


renderBeforePage : Config msg model -> State -> Int -> Page model -> List (Html msg)
renderBeforePage config state pageId page =
    page.model |> config.beforePageRenderer


wizardHeader : { a | namespace : Namespace, config : Config msg model } -> State -> SelectDict Int (Step model) -> Html msg
wizardHeader ({ namespace } as args) state steps =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    header [ class [ "WizardHeader" ] ]
        [ h2 [ class [ "WizardHeaderTitle" ] ]
            [ steps
                |> SelectDict.selectedValue
                |> getStepTitle
                |> text
            ]
        , navigation args state steps
        ]


stepIndicator : { a | namespace : Namespace, config : Config msg model } -> SelectDict Int (Step model) -> Html msg
stepIndicator { namespace, config } steps =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    div [ class [ "PageIndicator" ] ]
        [ text <| renderTemplate config steps config.pageIndicatorTemplate
        ]


navigationArrow : { a | namespace : Namespace, config : Config msg model } -> State -> Html msg
navigationArrow { namespace, config } state =
    let
        stateData =
            unwrap state

        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    case stateData.navigationStatus of
        ExpandedState ->
            button
                [ type_ "button"
                , class [ "NavigationArrow-Expanded" ]
                , onClick ({ stateData | navigationStatus = CollapsedState } |> State |> config.onState)
                , onBlur ({ stateData | navigationStatus = CollapsedState } |> State |> config.onState)
                ]
                [ Svg.chevron { namespace = namespace } [] ]

        CollapsedState ->
            button
                [ type_ "button"
                , class [ "NavigationArrow-Collapsed" ]
                , onClick ({ stateData | navigationStatus = ExpandedState } |> State |> config.onState)
                ]
                [ Svg.chevron { namespace = namespace } [] ]


navigation : { a | namespace : Namespace, config : Config msg model } -> State -> SelectDict Int (Step model) -> Html msg
navigation ({ namespace, config } as args) state steps =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        stateData =
            unwrap state

        ( before, selected, after ) =
            SelectDict.segments steps

        beforeMenuItems =
            before
                |> Dict.toList
                |> List.map (navigationStep { namespace = namespace, config = config, isBefore = True, isSelected = False, state = state })

        afterMenuItems =
            after
                |> Dict.toList
                |> List.map (navigationStep { namespace = namespace, config = config, isBefore = False, isSelected = False, state = state })

        currentMenuItem =
            selected
                |> navigationStep { namespace = namespace, config = config, isBefore = True, isSelected = True, state = state }
                |> List.singleton
    in
    case stateData.navigationStatus of
        CollapsedState ->
            nav [ class [ "Navigation" ] ]
                [ stepIndicator args steps
                , navigationArrow args state
                , ol
                    [ class [ "NavigationList-Collapsed" ]
                    , attribute "aria-hidden" "true"
                    ]
                    (beforeMenuItems
                        ++ currentMenuItem
                        ++ afterMenuItems
                    )
                ]

        ExpandedState ->
            nav [ class [ "Navigation" ] ]
                [ stepIndicator args steps
                , navigationArrow args state
                , ol
                    [ class [ "NavigationList-Expanded" ]
                    , onMouseDownPreventDefault (State { stateData | navigationStatus = ExpandedState } |> config.onState)
                    ]
                    (beforeMenuItems
                        ++ currentMenuItem
                        ++ afterMenuItems
                    )
                ]


navigationStep : { namespace : Namespace, config : Config msg model, isBefore : Bool, isSelected : Bool, state : State } -> ( Int, Step model ) -> Html msg
navigationStep { namespace, config, isBefore, isSelected, state } ( stepId, step ) =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace

        stateData =
            unwrap state
    in
    li
        [ class
            [ if isSelected then
                "NavigationItem-Selected"

              else
                "NavigationItem-NotSelected"
            ]
        , Html.Attributes.attribute "data-index" (String.fromInt (stepId + 1))
        ]
        [ if isBefore then
            a
                [ href "#"
                , onClick (config.onGotoStep { stepId = stepId } (State { stateData | navigationStatus = CollapsedState, inReview = shouldReview step config }))
                ]
                [ text <| getStepTitle step ]

          else
            text <| getStepTitle step
        ]


navigationControl : { a | namespace : Namespace, config : Config msg model, isLoading : Bool } -> State -> SelectDict Int (Step model) -> Html msg
navigationControl ({ namespace } as args) ((State _) as state) steps =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    div [ class [ "NavigationControl" ] ]
        [ previousButton args state steps
        , nextButton args state steps
        ]


previousButton : { a | namespace : Namespace, config : Config msg model, isLoading : Bool } -> State -> SelectDict Int (Step model) -> Html msg
previousButton { namespace, config } (State stateData) steps =
    let
        prev =
            steps |> SelectDict.getBefore |> Dict.toList |> List.reverse |> List.head
    in
    case prev of
        Nothing ->
            Html.Extra.nothing

        Just ( prevStepId, prevStep ) ->
            if shouldReview prevStep config then
                Button.standard
                    { attributes = [ onClick (config.onGotoStep { stepId = prevStepId } (State { stateData | inReview = True })) ]
                    , text = renderTemplate config steps config.prevButtonTemplate
                    , namespace = namespace
                    }

            else
                Button.standard
                    { attributes = [ onClick (config.onGotoStep { stepId = prevStepId } initialState) ]
                    , text = renderTemplate config steps config.prevButtonTemplate
                    , namespace = namespace
                    }


nextButton : { a | namespace : Namespace, config : Config msg model, isLoading : Bool } -> State -> SelectDict Int (Step model) -> Html msg
nextButton { namespace, config, isLoading } (State stateData) steps =
    let
        current =
            SelectDict.selectedValue steps

        next =
            getNextPage steps

        finishButton =
            Button.primary
                { attributes = [ onClick (config.onFinish (State { stateData | inReview = True })) ]
                , text = renderTemplate config steps config.finishButtonTemplate
                , namespace = namespace
                }

        nextPageButton pageId =
            Button.primary
                { attributes = [ onClick (config.onNextPage initialState) ]
                , text = renderTemplate config steps config.nextButtonTemplate
                , namespace = namespace
                }

        reviewStayInStepButton =
            Button.primary
                { attributes = [ onClick (config.onReview (State { stateData | inReview = True })) ]
                , text = renderTemplate config steps config.reviewButtonTemplate
                , namespace = namespace
                }

        nextStepInReviewButton =
            Button.primary
                { attributes = [ onClick (config.onNextStep (State { stateData | inReview = True })) ]
                , text = renderTemplate config steps config.nextButtonTemplate
                , namespace = namespace
                }

        infoNextButton =
            Button.primary
                { attributes = [ onClick (config.onNextStep initialState) ]
                , text =
                    if isLoading then
                        "Loading"

                    else
                        renderTemplate config steps config.nextButtonTemplate
                , namespace = namespace
                }

        formNextButton =
            Button.primary
                { attributes = [ onClick (config.onNextPage (State { stateData | inReview = False })) ]
                , text =
                    if isLoading then
                        "Loading"

                    else
                        renderTemplate config steps config.nextButtonTemplate
                , namespace = namespace
                }
    in
    case next of
        LastPage ->
            if shouldReview current config && not stateData.inReview then
                reviewStayInStepButton

            else
                finishButton

        NextPage ( pageId, _ ) ->
            nextPageButton pageId

        NextStep ( _, nextStep ) ->
            let
                nextButtonForFormPage =
                    if shouldReview current config && not stateData.inReview then
                        reviewStayInStepButton

                    else if shouldReview nextStep config then
                        nextStepInReviewButton

                    else
                        formNextButton
            in
            case current of
                Single singlePageValue ->
                    case singlePageValue.singlePageType of
                        SinglePageInfo ->
                            infoNextButton

                        SinglePageForm ->
                            nextButtonForFormPage

                Multi _ ->
                    nextButtonForFormPage


renderTemplate : Config msg model -> SelectDict Int (Step model) -> String -> String
renderTemplate config steps template =
    let
        before =
            SelectDict.getBefore steps

        totalSteps =
            SelectDict.toDict steps |> Dict.size

        currentStepIndex =
            Dict.size before + 1
    in
    Mustache.render
        [ Mustache.Variable "title" config.title
        , Mustache.Variable "nextTitle" (steps |> getNextTitle |> Maybe.withDefault "")
        , Mustache.Variable "prevTitle" (steps |> getPrevTitle |> Maybe.withDefault "")
        , Mustache.Variable "currentPage" (String.fromInt currentStepIndex)
        , Mustache.Variable "totalPage" (String.fromInt totalSteps)
        ]
        template


errorMessage : { a | namespace : Namespace, config : Config msg model, localize : String -> String } -> SelectDict Int (Step model) -> Html msg
errorMessage { namespace, localize } steps =
    let
        step =
            SelectDict.selectedValue steps
    in
    step
        |> getStepError
        |> Error.errorLocalized { namespace = namespace, localize = localize }



-- HELPERS


{-| Get the initial State
-}
initialState : State
initialState =
    State
        { navigationStatus = CollapsedState
        , inReview = False
        }


unwrap : State -> StateData
unwrap state =
    case state of
        State data ->
            data


{-| Get the Step title
-}
getStepTitle : Step model -> String
getStepTitle step =
    case step of
        Single { title } ->
            title

        Multi { title } ->
            title


{-| Get the page title
-}
getPageTitle : Step model -> String
getPageTitle step =
    case step of
        Single { title } ->
            title

        Multi { title, pages } ->
            if Dict.isEmpty (SelectDict.getBefore pages) then
                title

            else
                pages
                    |> SelectDict.selectedValue
                    |> .title


{-| Get the Step error Status
-}
getStepError : Step model -> Status
getStepError step =
    case step of
        Single { status } ->
            status

        Multi { pages } ->
            pages
                |> SelectDict.selectedValue
                |> .status


{-| Get the Step model
-}
getStepModel : Step model -> model
getStepModel step =
    case step of
        Single { model } ->
            model

        Multi { pages } ->
            pages
                |> SelectDict.selectedValue
                |> .model


type NextPageResult model
    = NextPage ( Int, Page model )
    | NextStep ( Int, Step model )
    | LastPage


getNextPage : SelectDict Int (Step model) -> NextPageResult model
getNextPage steps =
    let
        nextStep =
            steps
                |> getNextStep
                |> Maybe.map NextStep
                |> Maybe.withDefault LastPage
    in
    case SelectDict.selectedValue steps of
        Single _ ->
            nextStep

        Multi { pages } ->
            pages
                |> SelectDict.getAfter
                |> Dict.toList
                |> List.head
                |> Maybe.map NextPage
                |> Maybe.withDefault nextStep


getNextTitle : SelectDict Int (Step model) -> Maybe String
getNextTitle steps =
    case SelectDict.selectedValue steps of
        Single _ ->
            getNextStepTitle steps

        Multi { pages } ->
            pages
                |> SelectDict.getAfter
                |> Dict.values
                |> List.head
                |> Maybe.map .title
                |> Maybe.Extra.orElseLazy (\_ -> getNextStepTitle steps)


getNextStepTitle : SelectDict Int (Step model) -> Maybe String
getNextStepTitle steps =
    steps
        |> getNextStep
        |> Maybe.map (Tuple.second >> getStepTitle)


getNextStep : SelectDict Int (Step model) -> Maybe ( Int, Step model )
getNextStep steps =
    steps
        |> SelectDict.getAfter
        |> Dict.toList
        |> List.head


getPrevTitle : SelectDict Int (Step model) -> Maybe String
getPrevTitle steps =
    steps
        |> SelectDict.getBefore
        |> Dict.values
        |> List.reverse
        |> List.head
        |> Maybe.map getPageTitle


onMouseDownPreventDefault : msg -> Html.Attribute msg
onMouseDownPreventDefault msg =
    let
        eventOptions =
            { message = msg
            , preventDefault = True
            , stopPropagation = True
            }
    in
    Html.Events.custom "mousedown" (Json.Decode.succeed eventOptions)


shouldReview : Step model -> Config msg model -> Bool
shouldReview step { showReview } =
    case step of
        Single _ ->
            False

        Multi { pages } ->
            showReview && (pages |> SelectDict.getAfter |> Dict.isEmpty)
