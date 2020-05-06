module Lunarbox.Svg.Element
  ( tspan
  , withLabel
  ) where

import Halogen.HTML (ElemName(..), Node)
import Halogen.HTML as HH
import Svg.Elements (element)
import Svg.Elements as SE
import Svg.Indexed as I

-- The halogen-svg lib doesn't have this so I made my own
tspan :: forall p i. Node I.SVGtext p i
tspan = element (ElemName "tspan")

-- Helper to label an element using the <title> tag
withLabel :: forall h a. String -> HH.HTML h a -> HH.HTML h a
withLabel label content =
  SE.g []
    [ SE.title [] [ HH.text label ]
    , content
    ]
