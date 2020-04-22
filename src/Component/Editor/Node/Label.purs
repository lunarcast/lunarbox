module Lunarbox.Component.Editor.Node.Label
  ( label
  , labelText
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Svg.Attributes as SA
import Svg.Elements as SE

labelText :: forall h a. String -> HTML h a
labelText =
  SE.text
    [ SA.text_anchor SA.AnchorMiddle
    , SA.fill $ Just $ SA.RGB 63 196 255
    ]
    <<< pure
    <<< HH.text

label :: forall h a. HTML h a -> HTML h a
label inner =
  SE.text
    [ SA.text_anchor SA.AnchorMiddle
    , SA.fill $ Just $ SA.RGB 63 196 255
    ]
    [ inner ]
