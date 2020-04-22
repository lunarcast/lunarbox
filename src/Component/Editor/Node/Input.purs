module Lunarbox.Component.Editor.Node.Input
  ( input
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML (HTML)
import Lunarbox.Capability.Editor.Node.NodeInput (Arc(..))
import Lunarbox.Data.Editor.Constants (arcWidth)
import Lunarbox.Svg.Attributes (Linecap(..), arc, strokeLinecap, strokeWidth, transparent)
import Svg.Attributes (Color, D(..))
import Svg.Attributes as SA
import Svg.Elements as SE

type Input
  = { radius :: Number
    , spacing :: Number
    , arc :: Arc String
    , color :: Color
    }

input :: forall h a. Input -> HTML h a
input { radius, spacing, arc: Arc start end _, color } =
  SE.path
    [ SA.d $ Abs <$> arc radius (start + spacing) (end - spacing)
    , SA.fill $ Just transparent
    , SA.stroke $ Just color
    , strokeWidth arcWidth
    , strokeLinecap Round
    ]
