module Lunarbox.Capability.Foreign.Keymap (Keymap, mountKeymap, unmountKeymap) where

import Prelude
import Data.Tuple (Tuple)
import Effect (Effect)

type Keymap
  = Array (Tuple String (Effect Unit))

foreign import mountKeymap :: String -> Keymap -> Effect Unit

foreign import unmountKeymap :: String -> Effect Unit
