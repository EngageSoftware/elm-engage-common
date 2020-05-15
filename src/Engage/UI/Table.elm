module Engage.UI.Table exposing
    ( ActionColumnDetails
    , Column
    , ColumnAction(..)
    , Config
    , State
    , actionsColumn
    , customColumn
    , dateColumn
    , decreasingBy
    , decreasingOrIncreasingBy
    , floatColumn
    , formattedNumberColumn
    , formattedNumberWithLanguageColumn
    , increasingBy
    , increasingOrDecreasingBy
    , initialSort
    , intColumn
    , linkColumn
    , maybeColumn
    , stringColumn
    , table
    , unsortable
    , withCustomSorter
    )

import Date.Extra.Config.Configs as DateConfigs
import Date.Extra.Format exposing (format)
import Engage.DateHelper as DateHelper
import Engage.Namespace as Namespace exposing (Namespace)
import Engage.Styles.Class exposing (Importance(..))
import Engage.UI.Button as Button
import Engage.UI.Link as Link
import Engage.UI.Table.Css exposing (Class(..))
import Html exposing (Html)
import Html.Attributes
import Html.CssHelpers
import Html.Events exposing (onClick)
import Language as NumeralLanguage
import Numeral
import Table
import Time exposing (Time)


table : Config data msg -> State -> List data -> Html msg
table { namespace, toId, toMsg, columns } state data =
    let
        { class } =
            namespace
                |> Namespace.toString
                |> Html.CssHelpers.withNamespace

        config : Table.Config data msg
        config =
            Table.customConfig
                { toId = toId
                , toMsg = toMsg
                , columns = List.map (toSortableTableColumn namespace) columns
                , customizations =
                    { tableAttrs = [ class [ Table ] ]
                    , caption = Nothing
                    , thead = defaultCustomizations.thead
                    , tfoot = Nothing
                    , tbodyAttrs = []
                    , rowAttrs = always [ class [ DataRow ] ]
                    }
                }
    in
    Table.view config state data



-- Types


type alias Config data msg =
    { namespace : Namespace
    , toId : data -> String
    , toMsg : State -> msg
    , columns : List (Column data msg)
    }


type alias ActionColumnDetails data msg =
    { text : String
    , toMsg : data -> msg
    , importance : Importance
    , attributes : List (Html.Attribute msg)
    }


type ColumnAction data msg
    = PrimaryButton String (data -> msg) (List (Html.Attribute msg))
    | StandardButton String (data -> msg) (List (Html.Attribute msg))
    | DivertButton String (data -> msg) (List (Html.Attribute msg))



-- Aliases


type Column data msg
    = Column (ColumnData data msg)


type alias State =
    Table.State


type alias ColumnData data msg =
    { name : String
    , viewData : String -> data -> Table.HtmlDetails msg
    , sorter : Sorter data
    }


type alias Sorter data =
    Table.Sorter data


initialSort : String -> State
initialSort =
    Table.initialSort


unsortable : Sorter data
unsortable =
    Table.unsortable


increasingBy : (data -> comparable) -> Sorter data
increasingBy toComparable =
    Table.increasingBy toComparable


decreasingBy : (data -> comparable) -> Sorter data
decreasingBy toComparable =
    Table.decreasingBy toComparable


decreasingOrIncreasingBy : (data -> comparable) -> Sorter data
decreasingOrIncreasingBy toComparable =
    Table.decreasingOrIncreasingBy toComparable


increasingOrDecreasingBy : (data -> comparable) -> Sorter data
increasingOrDecreasingBy toComparable =
    Table.increasingOrDecreasingBy toComparable



-- Columns


stringColumn : String -> (data -> String) -> Column data msg
stringColumn name toStr =
    Column
        { name = name
        , viewData = always (toStr >> textDetails)
        , sorter = Table.increasingOrDecreasingBy toStr
        }


intColumn : String -> (data -> Int) -> Column data msg
intColumn name toInt =
    Column
        { name = name
        , viewData = always (toInt >> toString >> textDetails)
        , sorter = Table.increasingOrDecreasingBy toInt
        }


floatColumn : String -> (data -> Float) -> Column data msg
floatColumn name toFloat =
    Column
        { name = name
        , viewData = always (toFloat >> toString >> textDetails)
        , sorter = Table.increasingOrDecreasingBy toFloat
        }


dateColumn : String -> (data -> Time) -> String -> String -> Column data msg
dateColumn name toTime locale dateFormat =
    Column
        { name = name
        , viewData = always (toTime >> DateHelper.toDateIgnoreTimezone >> format (DateConfigs.getConfig locale) dateFormat >> textDetails)
        , sorter = Table.increasingOrDecreasingBy toTime
        }


linkColumn : String -> (data -> String) -> (data -> String) -> (data -> List (Html.Attribute msg)) -> Column data msg
linkColumn name toHref toText toAttributes =
    let
        linkAttributes : data -> List (Html.Attribute msg)
        linkAttributes data =
            Html.Attributes.href (toHref data) :: toAttributes data
    in
    Column
        { name = name
        , viewData = \namespace data -> linkDetails (linkAttributes data) (toText data) (Namespace.namespace namespace)
        , sorter = Table.increasingOrDecreasingBy toText
        }


formattedNumberColumn : String -> (data -> Float) -> String -> Column data msg
formattedNumberColumn name toCurrency format =
    Column
        { name = name
        , viewData = always (toCurrency >> Numeral.format format >> textDetails)
        , sorter = Table.increasingOrDecreasingBy toCurrency
        }


formattedNumberWithLanguageColumn : String -> (data -> Float) -> NumeralLanguage.Language -> String -> Column data msg
formattedNumberWithLanguageColumn name toCurrency language format =
    Column
        { name = name
        , viewData = always (toCurrency >> Numeral.formatWithLanguage language format >> textDetails)
        , sorter = Table.increasingOrDecreasingBy toCurrency
        }


actionsColumn : List (ColumnAction data msg) -> Column data msg
actionsColumn actions =
    Column
        { name = ""
        , viewData = \namespace data -> actionDetails actions (Namespace.namespace namespace) data
        , sorter = unsortable
        }


customColumn : String -> (data -> List (Html msg)) -> Column data msg
customColumn name toHtml =
    Column
        { name = name
        , viewData = always (toHtml >> Table.HtmlDetails [])
        , sorter = unsortable
        }


maybeColumn : String -> (data -> Maybe a) -> (a -> String) -> String -> Sorter data -> Column data msg
maybeColumn name toMaybeData toStr defaultValue sorter =
    let
        toStrValue : data -> String
        toStrValue data =
            case toMaybeData data of
                Just value ->
                    toStr value

                Nothing ->
                    defaultValue
    in
    Column
        { name = name
        , viewData = always (toStrValue >> textDetails)
        , sorter = sorter
        }



-- Helpers


defaultCustomizations : Table.Customizations data msg
defaultCustomizations =
    Table.defaultCustomizations


toSortableTableColumn : Namespace -> Column data msg -> Table.Column data msg
toSortableTableColumn namespace column =
    case column of
        Column { name, viewData, sorter } ->
            Table.veryCustomColumn
                { name = name
                , viewData = viewData <| Namespace.toString namespace
                , sorter = sorter
                }


actionDetails : List (ColumnAction data msg) -> Namespace -> data -> Table.HtmlDetails msg
actionDetails actions namespace data =
    let
        createTableAction : ColumnAction data msg -> Html msg
        createTableAction columnAction =
            case columnAction of
                PrimaryButton text toMsg attributes ->
                    Button.primarySmall
                        { namespace = namespace
                        , text = text
                        , attributes = onClick (toMsg data) :: attributes
                        }

                StandardButton text toMsg attributes ->
                    Button.standardSmall
                        { namespace = namespace
                        , text = text
                        , attributes = onClick (toMsg data) :: attributes
                        }

                DivertButton text toMsg attributes ->
                    Button.divertSmall
                        { namespace = namespace
                        , text = text
                        , attributes = onClick (toMsg data) :: attributes
                        }

        tableActions : List (Html msg)
        tableActions =
            List.map createTableAction actions
    in
    Table.HtmlDetails [] tableActions


textDetails : String -> Table.HtmlDetails msg
textDetails str =
    Table.HtmlDetails [] [ Html.text str ]


linkDetails : List (Html.Attribute msg) -> String -> Namespace -> Table.HtmlDetails msg
linkDetails attributes text namespace =
    let
        link =
            Link.standard
                { namespace = namespace
                , attributes = attributes
                , text = text
                }
    in
    Table.HtmlDetails [] [ link ]


withCustomSorter : Sorter data -> Column data msg -> Column data msg
withCustomSorter sorter column =
    case column of
        Column { name, viewData } ->
            Column
                { name = name
                , viewData = \namespace -> viewData namespace
                , sorter = sorter
                }
