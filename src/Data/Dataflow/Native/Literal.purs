module Lunarbox.Data.Dataflow.Native.Literal (true', false') where

import Prelude
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..))
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (typeBool)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))

true' :: NativeConfig
true' =
  NativeConfig
    { name: FunctionName "true"
    , expression: (NativeExpression (Forall [] typeBool) $ Bool true)
    , functionData: internal []
    }

false' :: NativeConfig
false' =
  NativeConfig
    { name: FunctionName "false"
    , expression: (NativeExpression (Forall [] typeBool) $ Bool false)
    , functionData: internal []
    }
