module Lunarbox.Data.Dataflow.Native.NativeConfig where

import Prelude
import Data.Foldable (class Foldable, foldr)
import Data.Lens (set)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Dataflow.FunctionName (FunctionName)
import Lunarbox.Data.Project (DataflowFunction(..), Project, _atProjectFunction)
import Lunarbox.Dataflow.Expression (NativeExpression)

newtype NativeConfig f
  = NativeConfig
  { functionData :: f
  , expression :: NativeExpression
  , name :: FunctionName
  }

loadNativeConfig :: forall f n. NativeConfig f -> Project f n -> Project f n
loadNativeConfig (NativeConfig { functionData, expression, name }) =
  set (_atProjectFunction name)
    $ Just
    $ Tuple (NativeFunction expression) functionData

-- I'm pretty proud of this one lol
loadNativeConfigs :: forall d n f. Foldable f => f (NativeConfig d) -> Project d n -> Project d n
loadNativeConfigs = flip $ foldr loadNativeConfig
