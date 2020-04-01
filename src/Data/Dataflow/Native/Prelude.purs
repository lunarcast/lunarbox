module Lunarbox.Data.Dataflow.Native.Prelude
  ( configs
  , loadPrelude
  ) where

import Lunarbox.Data.Dataflow.Native.Math (add, addTo)
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig, loadNativeConfigs)
import Lunarbox.Data.Editor.FunctionData (FunctionData)
import Lunarbox.Data.Editor.Project (Project)

configs :: Array (NativeConfig FunctionData)
configs = [ add, addTo ]

loadPrelude :: forall n. Project FunctionData n -> Project FunctionData n
loadPrelude = loadNativeConfigs configs
