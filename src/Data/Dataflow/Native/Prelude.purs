module Lunarbox.Data.Dataflow.Native.Prelude
  ( configs
  , preludeFunctionCount
  , customFunctionCount
  , loadPrelude
  ) where

import Prelude
import Data.Array as Array
import Data.Lens (view)
import Data.Map as Map
import Lunarbox.Data.Dataflow.Native.Array (arrayNodes)
import Lunarbox.Data.Dataflow.Native.ControlFlow (controlFlowNodes)
import Lunarbox.Data.Dataflow.Native.Function (functionNodes)
import Lunarbox.Data.Dataflow.Native.Literal (literalNodes)
import Lunarbox.Data.Dataflow.Native.Logic (logicNodes)
import Lunarbox.Data.Dataflow.Native.Math (mathNodes)
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig, loadNativeConfigs)
import Lunarbox.Data.Dataflow.Native.Predicate (predicateNodes)
import Lunarbox.Data.Dataflow.Native.String (stringNodes)
import Lunarbox.Data.Editor.State (State, _functionData)

-- Array wita s mll the built in nodes
configs :: forall a s m. Array (NativeConfig a s m)
configs =
  mathNodes
    <> functionNodes
    <> logicNodes
    <> stringNodes
    <> literalNodes
    <> controlFlowNodes
    <> arrayNodes
    <> predicateNodes

-- We need this to calculate the number of custom functions with ease
preludeFunctionCount :: Int
preludeFunctionCount = Array.length configs

-- Load all the built in nodes
loadPrelude :: forall a s m. State a s m -> State a s m
loadPrelude = loadNativeConfigs configs

-- Count all the user defined internal functions
customFunctionCount :: forall a s m. State a s m -> Int
customFunctionCount = (_ - preludeFunctionCount) <<< Map.size <<< view _functionData
