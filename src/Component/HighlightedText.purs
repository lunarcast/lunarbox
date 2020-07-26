module Lunarbox.Component.HighlightedText
  ( highlight
  , bold
  ) where

import Prelude
import CSS (Color)
import CSS as CSS
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)

highlightedClass :: String
highlightedClass = "highlighted"

-- Make some text of any color
highlight :: forall h a. Color -> HH.HTML h a -> HH.HTML h a
highlight color inner =
  HH.span
    [ style
        $ CSS.color
            color
    ]
    [ inner ]

-- Make some text bold and bigger (for highlighting small chars)
bold :: forall h a. HH.HTML h a -> HH.HTML h a
bold = HH.span [ style $ CSS.fontWeight CSS.bold ] <<< pure
