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
import Lunarbox.Data.Editor.State (State, _atFunctionData, _function)

newtype NativeConfig h a
  = NativeConfig
  { functionData :: FunctionData
  , expression :: NativeExpression
  , name :: FunctionName
  , component :: Maybe (FunctionUi h a)
  }

loadNativeConfig :: forall h a. NativeConfig h a -> State h a -> State h a
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
loadNativeConfigs :: forall h a f. Foldable f => f (NativeConfig h a) -> State h a -> State h a
loadNativeConfigs = flip $ foldr loadNativeConfig
