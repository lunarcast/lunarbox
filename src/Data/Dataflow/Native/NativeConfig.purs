module Lunarbox.Data.Dataflow.Native.NativeConfig where

import Prelude
import Lunarbox.Data.Dataflow.FunctionName (FunctionName(..))
import Lunarbox.Data.FunctionData (FunctionData(..))
import Lunarbox.Data.Project (Project)
import Lunarbox.Dataflow.Expression (NativeExpression(..))

newtype NativeConfig
  = NativeConfig
  { functionData :: FunctionData
  , expression :: NativeExpression
  , name :: FunctionName
  }

loadNativeConfig :: forall f n. NativeConfig -> Project f n -> Project f n
loadNativeConfig (NativeConfig { functionData, expression, name }) = identity
