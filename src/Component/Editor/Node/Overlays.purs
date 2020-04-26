module Lunarbox.Component.Editor.Node.Overlays
  ( overlays
  ) where

import Prelude
import Data.Array (mapWithIndex)
import Data.Int (toNumber)
import Halogen.HTML (HTML)
import Lunarbox.Data.Editor.Constants (nodeRadius)
import Svg.Attributes as SA
import Svg.Elements as SE

type Input h a
  = Array (HTML h a)

-- The text overlays on top of a node
overlays :: forall h a. Number -> Input h a -> HTML h a
overlays radius =
  SE.g [ SA.class_ "unselectable" ]
    <<< mapWithIndex \index elem ->
        SE.g
          [ SA.transform
              [ SA.Translate 0.0 $ -nodeRadius + (toNumber $ (index + 1) * -20)
              ]
          ]
          [ elem ]
