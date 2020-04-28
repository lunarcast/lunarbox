module Lunarbox.Data.Dataflow.Native.Math
  ( add
  ) where

import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (Type(..), typeNumber)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Prelude (($), (+))

addT :: Type
addT = TArrow typeNumber $ TArrow typeNumber typeNumber

addRuntimeValue :: RuntimeValue -> RuntimeValue -> RuntimeValue
addRuntimeValue (Number n) (Number n') = Number $ n + n'

addRuntimeValue _ _ = Null

add :: forall h a. NativeConfig h a
add =
  NativeConfig
    { name: FunctionName "add"
    , expression: (NativeExpression (Forall [] addT) $ binaryFunction addRuntimeValue)
    , functionData: internal [ { name: "a" }, { name: "b" } ]
    , component: Nothing
    }
