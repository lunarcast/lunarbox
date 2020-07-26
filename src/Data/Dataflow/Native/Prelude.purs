module Lunarbox.Data.Dataflow.Native.Prelude
  ( configs
  , loadPrelude
  ) where

import Prelude
import Lunarbox.Data.Dataflow.Native.Array (arrayNodes)
import Lunarbox.Data.Dataflow.Native.ControlFlow (controlFlowNodes)
import Lunarbox.Data.Dataflow.Native.Function (functionNodes)
import Lunarbox.Data.Dataflow.Native.Literal (literalNodes)
import Lunarbox.Data.Dataflow.Native.Logic (logicNodes)
import Lunarbox.Data.Dataflow.Native.Math (mathNodes)
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig, loadNativeConfigs)
import Lunarbox.Data.Dataflow.Native.Predicate (predicateNodes)
import Lunarbox.Data.Dataflow.Native.String (stringNodes)
import Lunarbox.Data.Editor.State (State)

-- Array wita s mll the built in nodes
configs :: Array (NativeConfig)
configs =
  mathNodes
    <> functionNodes
    <> logicNodes
    <> stringNodes
    <> literalNodes
    <> controlFlowNodes
    <> arrayNodes
    <> predicateNodes

-- Load all the built in nodes
loadPrelude :: State -> State
loadPrelude = loadNativeConfigs configs
