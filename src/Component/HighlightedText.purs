module Lunarbox.Component.HighlightedText (highlight) where

import Prelude
import CSS as CSS
import CSS as CSS.Color
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Svg.Attributes (Color(..))

highlightedClass :: String
highlightedClass = "highlighted"

svgColorToCssColor :: Color -> CSS.Color.Color
svgColorToCssColor (RGB r g b) = CSS.Color.rgb r g b

svgColorToCssColor (RGBA r g b a) = CSS.Color.rgba r g b a

highlight :: forall h a. Color -> HH.HTML h a -> HH.HTML h a
highlight color inner =
  HH.span
    [ style
        $ CSS.color
        $ svgColorToCssColor color
    ]
    [ inner ]
