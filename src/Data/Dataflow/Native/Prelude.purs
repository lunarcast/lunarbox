module Lunarbox.Data.Dataflow.Native.Prelude
  ( configs
  , loadPrelude
  ) where

import Lunarbox.Data.Dataflow.Native.ControlFlow (if')
import Lunarbox.Data.Dataflow.Native.Math (add)
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig, loadNativeConfigs)
import Lunarbox.Data.Editor.Project (Project)

configs :: Array NativeConfig
configs = [ add, if' ]

loadPrelude :: Project -> Project
loadPrelude = loadNativeConfigs configs
