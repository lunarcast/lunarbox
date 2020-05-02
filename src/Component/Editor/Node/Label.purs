module Lunarbox.Component.Editor.Node.Label
  ( label
  , labelText
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML (ComponentHTML, lazy)
import Halogen.HTML as HH
import Svg.Attributes as SA
import Svg.Elements as SE

labelText :: forall a s m. String -> ComponentHTML a s m
labelText =
  lazy
    $ SE.text
        [ SA.text_anchor SA.AnchorMiddle
        , SA.fill $ Just $ SA.RGB 63 196 255
        ]
    <<< pure
    <<< HH.text

-- A label displayed on top of a node
label :: forall a s m. ComponentHTML a s m -> ComponentHTML a s m
label =
  lazy
    $ SE.text
        [ SA.text_anchor SA.AnchorMiddle
        , SA.fill $ Just $ SA.RGB 63 196 255
        ]
    <<< pure
