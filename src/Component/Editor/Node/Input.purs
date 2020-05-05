module Lunarbox.Component.Editor.Node.Input
  ( input
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML (HTML)
import Halogen.HTML.Events (onClick)
import Lunarbox.Capability.Editor.Node.Arc (Arc(..))
import Lunarbox.Data.Editor.Constants (arcWidth)
import Lunarbox.Svg.Attributes (Linecap(..), arc, strokeLinecap, strokeWidth, transparent)
import Lunarbox.Svg.Element (withLabel)
import Svg.Attributes (Color, D(..))
import Svg.Attributes as SA
import Svg.Elements as SE

type Input a
  = { radius :: Number
    , spacing :: Number
    , arc :: Arc a
    , color :: Color
    , unconnectable :: Boolean
    , tooltip :: String
    }

input :: forall h a i. Input i -> Maybe a -> HTML h a
input { radius, spacing, tooltip, arc: Arc start end _, color, unconnectable } selectInput =
  withLabel tooltip
    $ SE.path
        [ SA.d $ Abs <$> arc radius (start + spacing) (end - spacing)
        , SA.fill $ Just transparent
        , SA.stroke $ Just color
        , SA.class_ $ "node-input" <> if unconnectable then " unconnectable" else ""
        , strokeWidth arcWidth
        , strokeLinecap Butt
        , onClick $ const selectInput
        ]
