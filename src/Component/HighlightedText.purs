module Lunarbox.Component.HighlightedText
  ( highlight
  , bold
  ) where

import Prelude
import CSS as CSS
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Svg.Attributes (Color(..))

highlightedClass :: String
highlightedClass = "highlighted"

svgColorToCssColor :: Color -> CSS.Color
svgColorToCssColor (RGB r g b) = CSS.rgb r g b

svgColorToCssColor (RGBA r g b a) = CSS.rgba r g b a

-- Make some text of any color
highlight :: forall h a. Color -> HH.HTML h a -> HH.HTML h a
highlight color inner =
  HH.span
    [ style
        $ CSS.color
        $ svgColorToCssColor color
    ]
    [ inner ]

-- Make some text bold and bigger (for highlighting small chars)
bold :: forall h a. HH.HTML h a -> HH.HTML h a
bold = HH.span [ style $ CSS.fontWeight CSS.bold ] <<< pure
