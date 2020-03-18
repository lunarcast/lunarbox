module Lunarbox.Component.Tooltip where

import Prelude
import Control.MonadZero (guard)
import Data.Array ((:))
import Data.Maybe (Maybe, fromMaybe, isJust)
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties (classes)
import Lunarbox.Component.Utils (className)

tooltip :: forall a b. Boolean -> HTML a b -> HTML a b -> HTML a b
tooltip active child content =
  HH.div [ classes $ ClassName <$> "tooltip-container" : ("active" <$ guard active) ]
    [ child
    , HH.div
        [ className "tooltip-content"
        ]
        [ content ]
    ]

maybeTooltip :: forall a b. Maybe (HTML a b) -> HTML a b -> HTML a b
maybeTooltip content child = tooltip (isJust content) child (fromMaybe (HH.text "") content)
