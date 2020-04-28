module Lunarbox.Data.Dataflow.Native.Function (pipe, identity', const') where

import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..))
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Prelude (const, identity, ($))

typePipe :: Scheme
typePipe =
  Forall [ input, output ] $ TArrow (TVarariable input)
    $ TArrow
        (TArrow (TVarariable input) (TVarariable output))
        (TVarariable output)
  where
  input = TVarName "i"

  output = TVarName "o"

evalPipe :: RuntimeValue -> RuntimeValue -> RuntimeValue
evalPipe input (Function function) = function input

evalPipe _ _ = Null

pipe :: forall h a. NativeConfig h a
pipe =
  NativeConfig
    { name: FunctionName "pipe"
    , expression: (NativeExpression typePipe $ binaryFunction evalPipe)
    , functionData:
      internal
        [ { name: "input" }
        , { name: "function" }
        ]
    , component: Nothing
    }

typeIdentity :: Scheme
typeIdentity = Forall [ input ] $ TArrow (TVarariable input) (TVarariable input)
  where
  input = TVarName "i"

identity' :: forall h a. NativeConfig h a
identity' =
  NativeConfig
    { name: FunctionName "identity"
    , expression: (NativeExpression typeIdentity $ Function identity)
    , functionData:
      internal
        [ { name: "input" }
        ]
    , component: Nothing
    }

typeConst :: Scheme
typeConst = Forall [ input, ignore ] $ TArrow (TVarariable input) $ TArrow (TVarariable ignore) (TVarariable input)
  where
  input = TVarName "input"

  ignore = TVarName "ignore"

const' :: forall h a. NativeConfig h a
const' =
  NativeConfig
    { name: FunctionName "const"
    , expression: (NativeExpression typeConst $ binaryFunction const)
    , functionData:
      internal
        [ { name: "constant value" }
        , { name: "ignored value" }
        ]
    , component: Nothing
    }
