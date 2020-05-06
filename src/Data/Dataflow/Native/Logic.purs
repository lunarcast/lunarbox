module Lunarbox.Data.Dataflow.Native.Logic
  ( evalNot
  , logicNodes
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (Type(..), typeBool)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))

-- All the native logic gatesish nodes
logicNodes :: forall a s m. Array (NativeConfig a s m)
logicNodes = [ not', or, and, xor ]

-- Evaluate a not function
evalNot :: RuntimeValue -> RuntimeValue
evalNot (Bool inner) = Bool $ not inner

evalNot _ = Null

-- Arguments for 2-inputs logic gates
binaryLogicFunctionArgs :: Array ({ name :: String })
binaryLogicFunctionArgs = [ { name: "first input" }, { name: "second input" } ]

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

not' :: forall a s m. NativeConfig a s m
not' =
  NativeConfig
    { name: FunctionName "not"
    , expression: NativeExpression (Forall [] $ typeFunction typeBool typeBool) $ Function evalNot
    , functionData: internal [ { name: "input" } ] { name: "!input" }
    , component: Nothing
    }

and :: forall a s m. NativeConfig a s m
and =
  NativeConfig
    { name: FunctionName "and"
    , expression: NativeExpression (Forall [] binaryLogicFunctionType) $ binaryLogicFunction (&&)
    , functionData: internal binaryLogicFunctionArgs { name: "a && b" }
    , component: Nothing
    }

or :: forall a s m. NativeConfig a s m
or =
  NativeConfig
    { name: FunctionName "or"
    , expression: NativeExpression (Forall [] binaryLogicFunctionType) $ binaryLogicFunction (||)
    , functionData: internal binaryLogicFunctionArgs { name: "a || b" }
    , component: Nothing
    }

evalXor :: Boolean -> Boolean -> Boolean
evalXor a b
  | a == b = false
  | otherwise = true

xor :: forall a s m. NativeConfig a s m
xor =
  NativeConfig
    { name: FunctionName "xor"
    , expression: NativeExpression (Forall [] binaryLogicFunctionType) $ binaryLogicFunction evalXor
    , functionData: internal binaryLogicFunctionArgs { name: "a & b" }
    , component: Nothing
    }
