module Lunarbox.Component.Editor.Edge
  ( renderEdge
  , Input
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Maybe (Maybe(..), maybe)
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Halogen.HTML (HTML)
import Halogen.HTML.Events (onClick)
import Lunarbox.Data.Editor.Constants (connectionsWidth)
import Lunarbox.Data.Vector (Vec2)
import Lunarbox.Svg.Attributes (strokeDashArray, strokeWidth)
import Svg.Attributes (Color)
import Svg.Attributes as SA
import Svg.Elements as SE

type Input
  = { from :: Vec2 Number
    , to :: Vec2 Number
    , color :: Color
    , dotted :: Boolean
    , className :: Maybe String
    }

type Actions a
  = { handleClick :: Maybe a
    }

-- Used to render the connections between nodes
renderEdge :: forall h a. Input -> Actions a -> HTML h a
renderEdge { from, to, color, dotted, className } { handleClick } =
  SE.line
    $ [ SA.x1 $ from !! d0
      , SA.y1 $ from !! d1
      , SA.x2 $ to !! d0
      , SA.y2 $ to !! d1
      , SA.stroke $ Just color
      , onClick $ const handleClick
      , strokeWidth connectionsWidth
      ]
    <> ( guard dotted
          $> strokeDashArray [ 10.0, 5.0 ]
      )
    <> (maybe mempty (pure <<< SA.class_) className)
