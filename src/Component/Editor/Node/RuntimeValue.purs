module Lunarbox.Component.Editor.RuntimeValue
  ( renderRuntimeValue
  , centeredText
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Svg.Attributes as SA
import Svg.Elements as SE

-- Helper to center some svg text
centeredText :: forall h a. SA.Color -> String -> HTML h a
centeredText color =
  SE.text
    [ SA.text_anchor SA.AnchorMiddle
    , SA.fill $ Just color
    , SA.class_ "unselectable"
    , SA.dominant_baseline SA.Hanging
    ]
    <<< pure
    <<< HH.text

-- render a value visually
renderRuntimeValue :: forall h a. SA.Color -> RuntimeValue -> HTML h a
renderRuntimeValue color = centeredText color <<< show
