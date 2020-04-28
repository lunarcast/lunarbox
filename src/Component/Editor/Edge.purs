module Lunarbox.Component.Editor.Edge
  ( renderEdge
  , Input
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), curry, uncurry)
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Halogen.HTML (ComponentHTML)
import Halogen.HTML as HH
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

renderEdge :: forall s a m. Input -> Actions a -> ComponentHTML a s m
renderEdge = curry $ HH.memoized (\(Tuple a _) (Tuple b _) -> a.from == b.from && a.to == b.to) renderEdge''

-- Used to render the connections between nodes
renderEdge' :: forall s a m. Input -> Actions a -> ComponentHTML a s m
renderEdge' { from, to, color, dotted, className } { handleClick } =
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

-- A version of renderEdge'' which takes a tuple instead of 2 arguments
renderEdge'' :: forall s a m. Tuple Input (Actions a) -> ComponentHTML a s m
renderEdge'' = uncurry renderEdge'
