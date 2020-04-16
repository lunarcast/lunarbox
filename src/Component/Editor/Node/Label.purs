module Lunarbox.Component.Editor.Node.Label
  ( label
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Svg.Attributes as SA
import Svg.Elements as SE

label :: forall h a. String -> HTML h a
label text =
  SE.text
    [ SA.text_anchor SA.AnchorMiddle
    , SA.fill $ Just $ SA.RGB 63 196 255
    ]
    [ HH.text text ]
