module Lunarbox.Component.Editor.Node.Overlays
  ( overlays
  ) where

import Prelude
import Data.Array (mapWithIndex)
import Data.Int (toNumber)
import Halogen.HTML (ComponentHTML, lazy2)
import Svg.Attributes as SA
import Svg.Elements as SE

type Input a s m
  = Array (ComponentHTML a s m)

-- The text overlays on top of a node
overlays' :: forall s a m. Number -> Input a s m -> ComponentHTML a s m
overlays' radius =
  SE.g [ SA.class_ "unselectable" ]
    <<< mapWithIndex \index elem ->
        SE.g
          [ SA.transform
              [ SA.Translate 0.0 $ -radius + (toNumber $ (index + 1) * -20)
              ]
          ]
          [ elem ]

-- Lazy version of overlays'
overlays :: forall s a m. Number -> Input a s m -> ComponentHTML a s m
overlays = lazy2 overlays'
