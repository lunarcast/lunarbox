module Lunarbox.Data.Dataflow.Native.NativeConfig where

import Prelude
import Data.Foldable (class Foldable, foldr)
import Data.Lens (set)
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (NativeExpression)
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction(..))
import Lunarbox.Data.Editor.FunctionData (FunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Project (Project, _atProjectFunction)

newtype NativeConfig
  = NativeConfig
  { functionData :: FunctionData
  , expression :: NativeExpression
  , name :: FunctionName
  }

loadNativeConfig :: NativeConfig -> Project -> Project
loadNativeConfig (NativeConfig { functionData, expression, name }) =
  set (_atProjectFunction name)
    $ Just
    $ (NativeFunction expression)

-- I'm pretty proud of this one lol
loadNativeConfigs :: forall f. Foldable f => f (NativeConfig) -> Project -> Project
loadNativeConfigs = flip $ foldr loadNativeConfig
