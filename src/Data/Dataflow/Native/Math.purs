module Lunarbox.Data.Dataflow.Native.Math
  ( mathNodes
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (Type(..), typeNumber)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Math (pow)

mathNodes :: forall a s m. Array (NativeConfig a s m)
mathNodes = [ add, substract, multiply, divide, raiseToPower ]

-- Type for functions of type Number -> Number -> Number
binaryNumberType :: Scheme
binaryNumberType = Forall [] $ TArrow typeNumber $ TArrow typeNumber typeNumber

-- Internal function used to perform the unwrapping and wrapping necessary for the binaryMathFUnction helper
binaryMathFunction' :: (Number -> Number -> Number) -> RuntimeValue -> RuntimeValue -> RuntimeValue
binaryMathFunction' function (Number first) (Number second) = Number $ function first second

binaryMathFunction' _ _ _ = Null

-- Helper for wrapping a purescript binary math operator into a runtime value
binaryMathFunction :: (Number -> Number -> Number) -> RuntimeValue
binaryMathFunction = binaryFunction <<< binaryMathFunction'

addRuntimeValue :: RuntimeValue -> RuntimeValue -> RuntimeValue
addRuntimeValue (Number n) (Number n') = Number $ n + n'

addRuntimeValue _ _ = Null

add :: forall a s m. NativeConfig a s m
add =
  NativeConfig
    { name: FunctionName "add"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction (+))
    , functionData: internal [ { name: "first number" }, { name: "second number" } ]
    , component: Nothing
    }

substract :: forall a s m. NativeConfig a s m
substract =
  NativeConfig
    { name: FunctionName "substract"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction (-))
    , functionData: internal [ { name: "first number" }, { name: "second number" } ]
    , component: Nothing
    }

multiply :: forall a s m. NativeConfig a s m
multiply =
  NativeConfig
    { name: FunctionName "multiply"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction (*))
    , functionData: internal [ { name: "first number" }, { name: "second number" } ]
    , component: Nothing
    }

divide :: forall a s m. NativeConfig a s m
divide =
  NativeConfig
    { name: FunctionName "divide"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction (/))
    , functionData: internal [ { name: "first number" }, { name: "second number" } ]
    , component: Nothing
    }

raiseToPower :: forall a s m. NativeConfig a s m
raiseToPower =
  NativeConfig
    { name: FunctionName "raise to power"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction pow)
    , functionData: internal [ { name: "base" }, { name: "exponend" } ]
    , component: Nothing
    }
