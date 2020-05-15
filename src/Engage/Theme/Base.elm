module Engage.Theme.Base exposing
    ( customStyle
    , spacing
    , style
    )

import Engage.Theme as Theme exposing (..)
import Engage.Unit.Border as Border exposing (..)
import Engage.Unit.FontFamily as FontFamily exposing (..)
import Engage.Unit.Margin as Margin exposing (..)
import Engage.Unit.Padding as Padding exposing (..)
import Engage.Unit.Relative as Relative exposing (..)
import Engage.Unit.Size as Size exposing (..)


spacing : Spacing
spacing =
    { button =
        { padding =
            { base = Padding.padding2 (em 1) (em 1.5)
            , small = Relative.relative 0.5
            }
        , margin =
            { base = margin2 (em 1) (em 0.5)
            , small = relative 1
            }
        }
    , input =
        { padding =
            { base = padding2 (em 0.75) (em 0.5)
            , small = relative 0
            }
        , margin =
            { base = margin4 (em 0.25) (em 0.25) (em 1) (em 0.25)
            , small = relative4 1 1 1 1
            }
        , labelPadding = baseLabel.labelPadding
        , labelMargin = baseLabel.labelMargin
        }
    , dropdown =
        { padding =
            { base = padding2 (em 0.75) (em 0.5)
            , small = relative 0
            }
        , margin =
            { base = margin4 (em 0.25) (em 0.25) (em 1) (em 0)
            , small = relative 1
            }
        , labelPadding = baseLabel.labelPadding
        , labelMargin = baseLabel.labelMargin
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


baseLabel : WithLabel {}
baseLabel =
    { labelPadding =
        { base = padding4 (em 0.75) (em 0.5) (em 0.75) zero
        , small = relative4 1 1 1 1
        }
    , labelMargin =
        { base = margin (px 0)
        , small = relative 0
        }
    }


style : Style
style =
    customStyle (em 1) FontFamily.notSet


customStyle : Size -> FontFamily -> Style
customStyle size fontFamily =
    let
        base =
            baseStyle size fontFamily
    in
    { button = base
    , input = base
    , dropdown = base
    , wizard =
        { fontSize = base.fontSize
        , fontFamily = FontFamily.notSet
        , border = Border.NotSet
        }
    , wizardHeader =
        { fontSize = base.fontSize
        , fontFamily = FontFamily.notSet
        , border = Border.notSet
        }
    }


baseStyle : Size -> FontFamily -> Typography (LabelTypography DecorationOnly)
baseStyle size fontFamily =
    { fontFamily = fontFamily
    , fontSize =
        { base = size
        , small = relative 0.85
        }
    , labelFontFamily = fontFamily
    , labelFontSize =
        { base = size
        , small = relative 0.85
        }
    , border = Border.notSet
    }
