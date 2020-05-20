module Engage.UI.Wizard exposing
    ( Config, Page, ShoppingCart, SinglePageType(..), State, Step
    , defaultConfig, getStepError, getStepModel, getStepTitle, initialState, multiPages, singlePage, wizard
    )

{-| UI.Wizard

@docs Config, Page, ShoppingCart, SinglePageType, State, Step

@docs defaultConfig, getStepError, getStepModel, getStepTitle, initialState, multiPages, singlePage, wizard

-}

import Dict exposing (Dict)
import Engage.CssHelpers
import Engage.Html.Extra as HtmlExtra
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.SelectDict as SelectDict exposing (SelectDict)
import Engage.UI.Button as Button
import Engage.UI.Error as Error exposing (Status)
import Engage.UI.Svg as Svg
import Engage.UI.Wizard.Css exposing (Class(..), NavigationStatus(..), SelectedStatus(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra
import Mustache



-- TYPES


{-| Wizard Step data
-}
type Step model comparable
    = Single { singlePageType : SinglePageType, title : String, status : Status, model : model }
    | Multi { title : String, pages : SelectDict comparable (Page model) }


{-| A SinglePageType type
-}
type SinglePageType
    = SinglePageInfo
    | SinglePageForm


{-| Get a single page Step
-}
singlePage : { singlePageType : SinglePageType, title : String, status : Status, model : model } -> Step model comparable
singlePage =
    Single


{-| Get a multi pages Step
-}
multiPages : { title : String, pages : SelectDict comparable (Page model) } -> Step model comparable
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
type alias Config msg model comparable =
    { pageIndicatorTemplate : String
    , nextButtonTemplate : String
    , prevButtonTemplate : String
    , finishButtonTemplate : String
    , reviewButtonTemplate : String
    , title : String
    , onNextStep : State -> msg
    , onGotoStep : { stepId : comparable } -> State -> msg
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
    , onGotoStep : { stepId : comparable } -> State -> msg
    , onNextPage : State -> msg
    , onState : State -> msg
    , onFinish : State -> msg
    , onReview : State -> msg
    }
    -> Config msg model comparable
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
    , config : Config msg model comparable
    , state : State
    , shoppingCart : ShoppingCart msg
    , isLoading : Bool
    , localize : String -> String
    }
    -> SelectDict comparable (Step model comparable)
    -> Html msg
wizard ({ namespace, config, state, shoppingCart, localize } as args) steps =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    section [ class [ Wizard ] ]
        ([ wizardHeader args state steps
         ]
            ++ (viewStep args state steps
                    :: [ errorMessage args steps
                       , navigationControl args state steps
                       ]
               )
            ++ [ shoppingCart.content
               ]
        )


viewStep : { a | namespace : Namespace, config : Config msg model comparable } -> State -> SelectDict comparable (Step model comparable) -> Html msg
viewStep { namespace, config } state steps =
    let
        step =
            SelectDict.selected steps

        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    case step of
        ( stepId, Single { model } ) ->
            div [ class [ WizardBody ] ] (config.stepRenderer model)

        ( stepId, Multi { pages } ) ->
            viewMultiPages
                { config = config
                , state = state
                , stepId = stepId
                , namespace = namespace
                }
                pages


viewMultiPages : { namespace : Namespace, config : Config msg model comparable, state : State, stepId : comparable } -> SelectDict comparable (Page model) -> Html msg
viewMultiPages { namespace, config, state, stepId } pages =
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
    div [ class [ WizardBody ] ]
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


renderBeforePage : Config msg model comparable -> State -> comparable -> Page model -> List (Html msg)
renderBeforePage config state pageId page =
    page.model |> config.beforePageRenderer


wizardHeader : { a | namespace : Namespace, config : Config msg model comparable } -> State -> SelectDict comparable (Step model comparable) -> Html msg
wizardHeader ({ namespace, config } as args) state steps =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    header [ class [ WizardHeader ] ]
        [ h2 [ class [ WizardHeaderTitle ] ]
            [ steps
                |> SelectDict.selectedValue
                |> getStepTitle
                |> text
            ]
        , navigation args state steps
        ]


stepIndicator : { a | namespace : Namespace, config : Config msg model comparable } -> SelectDict comparable (Step model comparable) -> Html msg
stepIndicator ({ namespace, config } as args) steps =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    div [ class [ PageIndicator ] ]
        [ text <| renderTemplate config steps config.pageIndicatorTemplate
        ]


navigationArrow : { a | namespace : Namespace, config : Config msg model comparable } -> State -> Html msg
navigationArrow ({ namespace, config } as args) state =
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
                , class [ NavigationArrow Expanded ]
                , onClick ({ stateData | navigationStatus = CollapsedState } |> State |> config.onState)
                , onBlur ({ stateData | navigationStatus = CollapsedState } |> State |> config.onState)
                ]
                [ Svg.chevron { namespace = namespace } [] ]

        CollapsedState ->
            button
                [ type_ "button"
                , class [ NavigationArrow Collapsed ]
                , onClick ({ stateData | navigationStatus = ExpandedState } |> State |> config.onState)
                ]
                [ Svg.chevron { namespace = namespace } [] ]


navigation : { a | namespace : Namespace, config : Config msg model comparable } -> State -> SelectDict comparable (Step model comparable) -> Html msg
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
            nav [ class [ Navigation ] ]
                [ stepIndicator args steps
                , navigationArrow args state
                , ol
                    [ class [ NavigationList Collapsed ]
                    , attribute "aria-hidden" "true"
                    ]
                    (beforeMenuItems
                        ++ currentMenuItem
                        ++ afterMenuItems
                    )
                ]

        ExpandedState ->
            nav [ class [ Navigation ] ]
                [ stepIndicator args steps
                , navigationArrow args state
                , ol
                    [ class [ NavigationList Expanded ]
                    , onMouseDownPreventDefault (State { stateData | navigationStatus = ExpandedState } |> config.onState)
                    ]
                    (beforeMenuItems
                        ++ currentMenuItem
                        ++ afterMenuItems
                    )
                ]


navigationStep : { namespace : Namespace, config : Config msg model comparable, isBefore : Bool, isSelected : Bool, state : State } -> ( comparable, Step model comparable ) -> Html msg
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
                NavigationItem Selected

              else
                NavigationItem NotSelected
            ]
        , Html.Attributes.attribute "data-index" (toString (stepId + 1))
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


navigationControl : { a | namespace : Namespace, config : Config msg model comparable, isLoading : Bool } -> State -> SelectDict comparable (Step model comparable) -> Html msg
navigationControl ({ namespace, config, isLoading } as args) ((State stateData) as state) steps =
    let
        class =
            namespace
                |> Namespace.toString
                |> Engage.CssHelpers.withNamespace
    in
    div [ class [ NavigationControl ] ]
        [ previousButton args state steps
        , nextButton args state steps
        ]


previousButton : { a | namespace : Namespace, config : Config msg model comparable, isLoading : Bool } -> State -> SelectDict comparable (Step model comparable) -> Html msg
previousButton ({ namespace, config, isLoading } as args) ((State stateData) as state) steps =
    let
        prev =
            steps |> SelectDict.before |> Dict.toList |> List.reverse |> List.head
    in
    case prev of
        Nothing ->
            HtmlExtra.none

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


nextButton : { a | namespace : Namespace, config : Config msg model comparable, isLoading : Bool } -> State -> SelectDict comparable (Step model comparable) -> Html msg
nextButton ({ namespace, config, isLoading } as args) ((State stateData) as state) steps =
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

        NextStep ( nextStepId, nextStep ) ->
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
                Single singlePage ->
                    case singlePage.singlePageType of
                        SinglePageInfo ->
                            infoNextButton

                        SinglePageForm ->
                            nextButtonForFormPage

                Multi _ ->
                    nextButtonForFormPage


renderTemplate : Config msg model comparable -> SelectDict comparable (Step model comparable) -> String -> String
renderTemplate config steps template =
    let
        next =
            SelectDict.after steps |> Dict.values |> List.head

        before =
            SelectDict.before steps

        totalSteps =
            SelectDict.toDict steps |> Dict.size

        currentStepIndex =
            Dict.size before + 1
    in
    Mustache.render
        [ Mustache.Variable "title" config.title
        , Mustache.Variable "nextTitle" (steps |> getNextTitle |> Maybe.withDefault "")
        , Mustache.Variable "prevTitle" (steps |> getPrevTitle |> Maybe.withDefault "")
        , Mustache.Variable "currentPage" (toString currentStepIndex)
        , Mustache.Variable "totalPage" (toString totalSteps)
        ]
        template


errorMessage : { a | namespace : Namespace, config : Config msg model comparable, localize : String -> String } -> SelectDict comparable (Step model comparable) -> Html msg
errorMessage { namespace, config, localize } steps =
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
getStepTitle : Step model comparable -> String
getStepTitle step =
    case step of
        Single { title } ->
            title

        Multi { title, pages } ->
            title


{-| Get the page title
-}
getPageTitle : Step model comparable -> String
getPageTitle step =
    case step of
        Single { title } ->
            title

        Multi { title, pages } ->
            if Dict.isEmpty (SelectDict.before pages) then
                title

            else
                pages
                    |> SelectDict.selectedValue
                    |> .title


{-| Get the Step error Status
-}
getStepError : Step model comparable -> Status
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
getStepModel : Step model comparable -> model
getStepModel step =
    case step of
        Single { model } ->
            model

        Multi { pages } ->
            pages
                |> SelectDict.selectedValue
                |> .model


type NextPageResult comparable model
    = NextPage ( comparable, Page model )
    | NextStep ( comparable, Step model comparable )
    | LastPage


getNextPage : SelectDict comparable (Step model comparable) -> NextPageResult comparable model
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
                |> SelectDict.after
                |> Dict.toList
                |> List.head
                |> Maybe.map NextPage
                |> Maybe.withDefault nextStep


getNextTitle : SelectDict comparable (Step model comparable) -> Maybe String
getNextTitle steps =
    case SelectDict.selectedValue steps of
        Single _ ->
            getNextStepTitle steps

        Multi { pages } ->
            pages
                |> SelectDict.after
                |> Dict.values
                |> List.head
                |> Maybe.map .title
                |> Maybe.Extra.orElseLazy (\_ -> getNextStepTitle steps)


getNextStepTitle : SelectDict comparable (Step model comparable) -> Maybe String
getNextStepTitle steps =
    steps
        |> getNextStep
        |> Maybe.map (Tuple.second >> getStepTitle)


getNextStep : SelectDict comparable (Step model comparable) -> Maybe ( comparable, Step model comparable )
getNextStep steps =
    steps
        |> SelectDict.after
        |> Dict.toList
        |> List.head


isFirstPage : SelectDict comparable (Page mode) -> Bool
isFirstPage pages =
    pages |> SelectDict.before |> Dict.isEmpty


getPrevTitle : SelectDict comparable (Step model comparable) -> Maybe String
getPrevTitle steps =
    steps
        |> SelectDict.before
        |> Dict.values
        |> List.reverse
        |> List.head
        |> Maybe.map getPageTitle


onMouseDownPreventDefault : msg -> Html.Attribute msg
onMouseDownPreventDefault msg =
    let
        eventOptions =
            { preventDefault = True
            , stopPropagation = True
            }
    in
    Html.Events.onWithOptions "mousedown" eventOptions (Json.Decode.succeed msg)


shouldReview : Step model comparable -> Config msg model comparable -> Bool
shouldReview step { showReview } =
    case step of
        Single page ->
            False

        Multi { pages } ->
            showReview && (pages |> SelectDict.after |> Dict.isEmpty)
