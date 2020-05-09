module Lunarbox.Data.Dataflow.Native.Math
  ( mathNodes
  ) where

import Prelude
import Math (sqrt)
import Data.Maybe (Maybe(..))
import Data.Number (isNaN)
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (typeFunction, typeNumber)
import Lunarbox.Data.Editor.FunctionData (PinDoc, internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Math (pow, (%))

-- ALl the math native nodes
mathNodes :: forall a s m. Array (NativeConfig a s m)
mathNodes = [ add, subtract, multiply, divide, raiseToPower, modulus, squareRoot ]

-- Type for functions of type Number -> Number -> Number
binaryNumberType :: Scheme
binaryNumberType = Forall [] $ typeFunction typeNumber $ typeFunction typeNumber typeNumber

-- Internal function used to perform the unwrapping and wrapping necessary for the binaryMathFUnction helper
binaryMathFunction' :: (Number -> Number -> Number) -> RuntimeValue -> RuntimeValue -> RuntimeValue
binaryMathFunction' function (Number first) (Number second) =
  if isNaN result then
    Null
  else
    Number result
  where
  result = function first second

binaryMathFunction' _ _ _ = Null

-- Documentation for an numeral value
numberDoc :: String -> PinDoc
numberDoc = { name: _, description: "Any numeric value" }

-- Helper for wrapping a purescript binary math operator into a runtime value
binaryMathFunction :: (Number -> Number -> Number) -> RuntimeValue
binaryMathFunction = binaryFunction <<< binaryMathFunction'

-- The actual math functions
add :: forall a s m. NativeConfig a s m
add =
  NativeConfig
    { name: FunctionName "add"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction (+))
    , functionData: internal [ numberDoc "first number", numberDoc "second number" ] { name: "sum", description: "The result of adding both arguments" }
    , component: Nothing
    }

subtract :: forall a s m. NativeConfig a s m
subtract =
  NativeConfig
    { name: FunctionName "subtract"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction (-))
    , functionData:
        internal [ numberDoc "first number", numberDoc "second number" ]
          { name: "difference", description: "The result of subtracting the second argument from the first" }
    , component: Nothing
    }

multiply :: forall a s m. NativeConfig a s m
multiply =
  NativeConfig
    { name: FunctionName "multiply"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction (*))
    , functionData:
        internal [ numberDoc "first number", numberDoc "second number" ]
          { name: "product", description: "The result of multiplying the first number by the second" }
    , component: Nothing
    }

divide :: forall a s m. NativeConfig a s m
divide =
  NativeConfig
    { name: FunctionName "divide"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction (/))
    , functionData:
        internal
          [ { name: "dividend", description: "The number to divide" }
          , { name: "divisor", description: "The number to divide the dividend by. Cannot be 0" }
          ]
          { name: "quotient"
          , description: "The result of dividng the first argument by the second"
          }
    , component: Nothing
    }

raiseToPower :: forall a s m. NativeConfig a s m
raiseToPower =
  NativeConfig
    { name: FunctionName "raise to power"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction pow)
    , functionData:
        internal [ numberDoc "base", numberDoc "exponend" ]
          { name: "base^exponent", description: "The result of raising the first argument to the power of the second" }
    , component: Nothing
    }

modulus :: forall a s m. NativeConfig a s m
modulus =
  NativeConfig
    { name: FunctionName "modulus"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction (%))
    , functionData:
        internal
          [ { name: "left side", description: "The number to take the modulus from" }
          , { name: "right side", description: "The number to divide the first input by and find the remainder" }
          ]
          { name: "a % b", description: "The remainder of dividing the first number to the second" }
    , component: Nothing
    }

evalSqrt :: RuntimeValue -> RuntimeValue
evalSqrt (Number num) = if num >= 0.0 then Number $ sqrt num else Null

evalSqrt _ = Null

squareRoot :: forall a s m. NativeConfig a s m
squareRoot =
  NativeConfig
    { name: FunctionName "square root"
    , expression: NativeExpression (Forall [] $ typeFunction typeNumber typeNumber) $ Function evalSqrt
    , functionData:
        internal
          [ { name: "radicand", description: "The number to take the square root from" } ]
          { name: "sqrt a", description: "The result of taking the square root of the number" }
    , component: Nothing
    }
