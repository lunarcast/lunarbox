module Lunarbox.Data.Dataflow.Native.NativeConfig where

import Prelude
import Data.Foldable (class Foldable, foldr)
import Data.Lens (set)
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (NativeExpression)
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction(..))
import Lunarbox.Data.Editor.FunctionData (FunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.FunctionUi (FunctionUi)
import Lunarbox.Data.Editor.State (State, _atFunctionData, _function, _ui)

newtype NativeConfig a s m
  = NativeConfig
  { functionData :: FunctionData
  , expression :: NativeExpression
  , name :: FunctionName
  , component :: Maybe (FunctionUi a s m)
  }

loadNativeConfig :: forall a s m. NativeConfig a s m -> State a s m -> State a s m
loadNativeConfig (NativeConfig { functionData, expression, name, component }) = loadFunction <<< loadFunctionData <<< loadFunctionUi
  where
  loadFunction =
    set (_function name)
      $ Just
      $ (NativeFunction expression)

  loadFunctionUi = set (_ui name) component

  loadFunctionData =
    set (_atFunctionData name)
      $ Just
      $ functionData

-- I'm pretty proud of this one lol
loadNativeConfigs :: forall a s m f. Foldable f => f (NativeConfig a s m) -> State a s m -> State a s m
loadNativeConfigs = flip $ foldr loadNativeConfig
