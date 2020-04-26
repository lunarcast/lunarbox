module Lunarbox.Component.Editor.Edge
  ( renderEdge
  , Input
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Halogen.HTML (HTML)
import Lunarbox.Data.Editor.Constants (connectionsWidth)
import Lunarbox.Data.Vector (Vec2)
import Lunarbox.Svg.Attributes (strokeWidth)
import Svg.Attributes (Color)
import Svg.Attributes as SA
import Svg.Elements as SE

type Input
  = { from :: Vec2 Number
    , to :: Vec2 Number
    , color :: Color
    }

-- Used to render the connections between nodes
renderEdge :: forall h a. Input -> HTML h a
renderEdge { from, to, color } =
  SE.line
    [ SA.x1 $ from !! d0
    , SA.y1 $ from !! d1
    , SA.x2 $ to !! d0
    , SA.y2 $ to !! d1
    , SA.stroke $ Just color
    , strokeWidth connectionsWidth
    ]
