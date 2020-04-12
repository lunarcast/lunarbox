module Lunarbox.Data.Dataflow.Native.Prelude
  ( configs
  , loadPrelude
  ) where

import Lunarbox.Data.Dataflow.Native.ControlFlow (if')
import Lunarbox.Data.Dataflow.Native.Math (add)
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig, loadNativeConfigs)
import Lunarbox.Data.Editor.FunctionData (FunctionData)
import Lunarbox.Data.Editor.Project (Project)

configs :: Array (NativeConfig FunctionData)
configs = [ add, if' ]

loadPrelude :: forall n. Project FunctionData n -> Project FunctionData n
loadPrelude = loadNativeConfigs configs
