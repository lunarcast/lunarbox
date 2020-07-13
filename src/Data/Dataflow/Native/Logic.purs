module Lunarbox.Data.Dataflow.Native.Logic
  ( evalNot
  , logicNodes
  ) where

import Prelude
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (Type, typeBool, typeFunction)
import Lunarbox.Data.Editor.FunctionData (PinDoc, internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))

-- All the native logic gatesish nodes
logicNodes :: Array (NativeConfig)
logicNodes = [ not', or, and, xor ]

-- Evaluate a not function
evalNot :: RuntimeValue -> RuntimeValue
evalNot (Bool inner) = Bool $ not inner

evalNot _ = Null

-- Arguments for 2-inputs logic gates
binaryLogicFunctionArgs :: Array PinDoc
binaryLogicFunctionArgs = [ { name: "first input", description: "Any boolean" }, { name: "second input", description: "Any boolean" } ]

-- Wrap a logic binary function into a binary function operating on Runtime values
binaryLogicFunction' :: (Boolean -> Boolean -> Boolean) -> RuntimeValue -> RuntimeValue -> RuntimeValue
binaryLogicFunction' function (Bool a) (Bool b) = Bool $ function a b

binaryLogicFunction' _ _ _ = Null

-- Wrap a logic binary function into a Runtime value
binaryLogicFunction :: (Boolean -> Boolean -> Boolean) -> RuntimeValue
binaryLogicFunction = binaryFunction <<< binaryLogicFunction'

-- The type for binary logic functions
binaryLogicFunctionType :: Type
binaryLogicFunctionType = typeFunction typeBool $ typeFunction typeBool typeBool

not' :: NativeConfig
not' =
  NativeConfig
    { name: FunctionName "not"
    , expression: NativeExpression (Forall [] $ typeFunction typeBool typeBool) $ Function evalNot
    , functionData: internal [ { name: "input", description: "Any boolean" } ] { name: "!input", description: "If the input is true returns false, else returns true" }
    }

and :: NativeConfig
and =
  NativeConfig
    { name: FunctionName "and"
    , expression: NativeExpression (Forall [] binaryLogicFunctionType) $ binaryLogicFunction (&&)
    , functionData: internal binaryLogicFunctionArgs { name: "a && b", description: "Returns true if both of the inputs are true" }
    }

or :: NativeConfig
or =
  NativeConfig
    { name: FunctionName "or"
    , expression: NativeExpression (Forall [] binaryLogicFunctionType) $ binaryLogicFunction (||)
    , functionData: internal binaryLogicFunctionArgs { name: "a || b", description: "Returns true if at least one of the inputs is true" }
    }

evalXor :: Boolean -> Boolean -> Boolean
evalXor a b
  | a == b = false
  | otherwise = true

xor :: NativeConfig
xor =
  NativeConfig
    { name: FunctionName "xor"
    , expression: NativeExpression (Forall [] binaryLogicFunctionType) $ binaryLogicFunction evalXor
    , functionData: internal binaryLogicFunctionArgs { name: "a ^ b", description: "Returns true if one and only one of the inputs is true" }
    }
