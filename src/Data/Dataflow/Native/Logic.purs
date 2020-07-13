module Lunarbox.Data.Dataflow.Native.Logic
  ( logicNodes
  ) where

import Prelude
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime.Class.Describable (toNativeExpression)
import Lunarbox.Data.Editor.FunctionData (PinDoc, internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))

-- All the native logic gatesish nodes
logicNodes :: Array (NativeConfig)
logicNodes = [ not', or, and, xor ]

-- Arguments for 2-inputs logic gates
binaryLogicFunctionArgs :: Array PinDoc
binaryLogicFunctionArgs = [ { name: "first input", description: "Any boolean" }, { name: "second input", description: "Any boolean" } ]

not' :: NativeConfig
not' =
  NativeConfig
    { name: FunctionName "not"
    , expression: toNativeExpression (not :: Boolean -> _)
    , functionData: internal [ { name: "input", description: "Any boolean" } ] { name: "!input", description: "If the input is true returns false, else returns true" }
    }

and :: NativeConfig
and =
  NativeConfig
    { name: FunctionName "and"
    , expression: toNativeExpression ((&&) :: Boolean -> _)
    , functionData: internal binaryLogicFunctionArgs { name: "a && b", description: "Returns true if both of the inputs are true" }
    }

or :: NativeConfig
or =
  NativeConfig
    { name: FunctionName "or"
    , expression: toNativeExpression ((||) :: Boolean -> _)
    , functionData: internal binaryLogicFunctionArgs { name: "a || b", description: "Returns true if at least one of the inputs is true" }
    }

xor :: NativeConfig
xor =
  NativeConfig
    { name: FunctionName "xor"
    , expression: toNativeExpression ((/=) :: Boolean -> _)
    , functionData: internal binaryLogicFunctionArgs { name: "a ^ b", description: "Returns true if one and only one of the inputs is true" }
    }
