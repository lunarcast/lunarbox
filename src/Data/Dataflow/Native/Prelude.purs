module Lunarbox.Data.Dataflow.Native.Prelude
  ( configs
  , loadPrelude
  ) where

import Lunarbox.Data.Dataflow.Native.ControlFlow (if')
import Lunarbox.Data.Dataflow.Native.Function (const', identity', pipe)
import Lunarbox.Data.Dataflow.Native.Literal (false', true')
import Lunarbox.Data.Dataflow.Native.Math (add)
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig, loadNativeConfigs)
import Lunarbox.Data.Editor.State (State)

-- Array with all the built in nodes
configs :: forall h a. Array (NativeConfig h a)
configs = [ add, if', pipe, identity', const', true', false' ]

-- Load all the built in nodes
loadPrelude :: forall h a. State h a -> State h a
loadPrelude = loadNativeConfigs configs
