module Lunarbox.Data.Dataflow.Native.NativeConfig where

import Prelude
import Data.Foldable (class Foldable, foldr)
import Data.Lens (set)
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (NativeExpression)
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction(..))
import Lunarbox.Data.Editor.FunctionData (FunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.State (State, _atFunctionData, _function)

newtype NativeConfig
  = NativeConfig
  { functionData :: FunctionData
  , expression :: NativeExpression
  , name :: FunctionName
  }

loadNativeConfig :: NativeConfig -> State -> State
loadNativeConfig (NativeConfig { functionData, expression, name }) = loadFunction <<< loadFunctionData
  where
  loadFunction =
    set (_function name)
      $ Just
      $ (NativeFunction expression)

  loadFunctionData =
    set (_atFunctionData name)
      $ Just
      $ functionData

-- I'm pretty proud of this one lol
loadNativeConfigs :: forall f. Foldable f => f (NativeConfig) -> State -> State
loadNativeConfigs = flip $ foldr loadNativeConfig
