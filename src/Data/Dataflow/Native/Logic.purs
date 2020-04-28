module Lunarbox.Data.Dataflow.Native.Logic (evalNot, not') where

import Prelude
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..))
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (typeBool)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))

evalNot :: RuntimeValue -> RuntimeValue
evalNot (Bool inner) = Bool $ not inner

evalNot _ = Null

not' :: forall h a. NativeConfig h a
not' =
  NativeConfig
    { name: FunctionName "name"
    , expression: NativeExpression (Forall [] typeBool) $ Function evalNot
    , functionData: internal [ { name: "input" } ]
    , component: Nothing
    }
