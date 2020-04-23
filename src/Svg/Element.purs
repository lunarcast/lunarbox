module Lunarbox.Svg.Element
  ( tspan
  ) where

import Halogen.HTML (ElemName(..), Node)
import Svg.Indexed as I
import Svg.Elements (element)

-- The halogen-svg lib doesn't have this so I made my own
tspan :: forall p i. Node I.SVGtext p i
tspan = element (ElemName "tspan")
