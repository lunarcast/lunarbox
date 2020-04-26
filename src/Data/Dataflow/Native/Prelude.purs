module Lunarbox.Data.Dataflow.Native.Prelude
  ( configs
  , loadPrelude
  ) where

import Lunarbox.Data.Dataflow.Native.ControlFlow (if')
import Lunarbox.Data.Dataflow.Native.Function (const', identity', pipe)
import Lunarbox.Data.Dataflow.Native.Math (add)
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig, loadNativeConfigs)
import Lunarbox.Data.Editor.State (State)

configs :: Array NativeConfig
configs = [ add, if', pipe, identity', const' ]

loadPrelude :: State -> State
loadPrelude = loadNativeConfigs configs
