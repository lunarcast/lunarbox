module Lunarbox.Capability.Foreign.Keymap (Keymap, mountKeymap, unmountKeymap) where

import Prelude
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class (class MonadEffect)

type Keymap m
  = Array (Tuple String (m Unit))

foreign import mountKeymap :: forall m. MonadEffect m => String -> Keymap m -> m Unit

foreign import unmountKeymap :: String -> Effect Unit
