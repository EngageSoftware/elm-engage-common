module Engage.Theme.None exposing
    ( palette
    , spacing
    , style
    )

import Css exposing (hex)
import Engage.Theme as Theme exposing (..)
import Engage.Unit.Border as Border exposing (..)
import Engage.Unit.Color as Color exposing (color)
import Engage.Unit.FontFamily as FontFamily exposing (..)
import Engage.Unit.Margin as Margin exposing (..)
import Engage.Unit.Padding as Padding exposing (..)
import Engage.Unit.Relative as Relative exposing (..)
import Engage.Unit.Size as Size exposing (..)


palette : Palette
palette =
    { error = { base = color <| hex "#DC3A21", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , warning = { base = color <| hex "#FE9839", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , confirmation = { base = color <| hex "#3CB180", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#FFFFFF" }
    , info = { base = color <| hex "#284783", contrast = color <| hex "#FFFFFF", tertiary = color <| hex "#5d83cd" }
    , buttonPrimary = { base = Color.notSet, contrast = Color.notSet, tertiary = Color.notSet }
    , buttonPrimaryHover = { base = Color.notSet, contrast = Color.notSet, tertiary = Color.notSet }
    , buttonStandard = { base = Color.notSet, contrast = Color.notSet, tertiary = Color.notSet }
    , buttonStandardHover = { base = Color.notSet, contrast = Color.notSet, tertiary = Color.notSet }
    , buttonDivert = { base = Color.notSet, contrast = Color.notSet, tertiary = Color.notSet }
    , buttonDivertHover = { base = Color.notSet, contrast = Color.notSet, tertiary = Color.notSet }
    , input = { base = Color.notSet, contrast = Color.notSet, tertiary = Color.notSet }
    , dropdown = { base = Color.notSet, contrast = Color.notSet, tertiary = Color.notSet }
    , wizardHeader = { base = Color.notSet, contrast = Color.notSet, tertiary = Color.notSet }
    }


spacing : Spacing
spacing =
    { button =
        { padding =
            { base = Padding.notSet
            , small = relative 1
            }
        , margin =
            { base = Margin.notSet
            , small = relative 1
            }
        }
    , input =
        { padding =
            { base = Padding.notSet
            , small = relative 1
            }
        , margin =
            { base = Margin.notSet
            , small = relative 1
            }
        , labelPadding =
            { base = Padding.notSet
            , small = relative 1
            }
        , labelMargin =
            { base = Margin.notSet
            , small = relative 1
            }
        }
    , dropdown =
        { padding =
            { base = Padding.notSet
            , small = relative 1
            }
        , margin =
            { base = Margin.notSet
            , small = relative 1
            }
        , labelPadding =
            { base = Padding.notSet
            , small = relative 1
            }
        , labelMargin =
            { base = Margin.notSet
            , small = relative 1
            }
        }
    , wizard =
        { padding =
            { base = padding3 (px 0) (em 1) (em 4)
            , small = relative 0
            }
        , margin =
            { base = margin2 (em 0) auto
            , small = relative 1
            }
        }
    , wizardHeader =
        { padding =
            { base = padding (em 0.5)
            , small = relative 0
            }
        , margin =
            { base = margin2 (em 2) (px 0)
            , small = relative 1
            }
        }
    }


style : Style
style =
    let
        base =
            { fontFamily = FontFamily.notSet
            , fontSize =
                { base = Size.notSet
                , small = relative 1
                }
            , labelFontFamily = FontFamily.notSet
            , labelFontSize =
                { base = Size.notSet
                , small = relative 1
                }
            , border = Border.notSet
            }
    in
    { button = base
    , input = base
    , dropdown = base
    , wizard =
        { fontSize = base.fontSize
        , fontFamily = FontFamily.notSet
        , border = Border.notSet
        }
    , wizardHeader =
        { fontSize = base.fontSize
        , fontFamily = FontFamily.notSet
        , border = Border.notSet
        }
    }
