module Lunarbox.Data.Dataflow.Native.Math
  ( mathNodes
  ) where

import Prelude
import Data.Number (isNaN)
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (typeFunction, typeNumber)
import Lunarbox.Data.Editor.FunctionData (PinDoc, internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Math (pow, sqrt, (%))

-- ALl the math native nodes
mathNodes :: Array (NativeConfig)
mathNodes = [ add, subtract, multiply, divide, raiseToPower, modulus, squareRoot, pred, succ ]

-- Type for functions of type Number -> Number -> Number
binaryNumberType :: Scheme
binaryNumberType = Forall [] $ typeFunction typeNumber $ typeFunction typeNumber typeNumber

-- Same but for Number -> Number
unaryNumberType :: Scheme
unaryNumberType = Forall [] $ typeFunction typeNumber typeNumber

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

-- Internal version of unaryMathFunction
unaryMathFunction' :: (Number -> Number) -> RuntimeValue -> RuntimeValue
unaryMathFunction' function (Number first) = Number $ function first

unaryMathFunction' _ _ = Null

-- Documentation for an numeral value
numberDoc :: String -> PinDoc
numberDoc = { name: _, description: "Any numeric value" }

-- Helper for wrapping a purescript binary math operator into a runtime value
binaryMathFunction :: (Number -> Number -> Number) -> RuntimeValue
binaryMathFunction = binaryFunction <<< binaryMathFunction'

-- Helper for wrapping a purescript unary math operator into a runtime value
unaryMathFunction :: (Number -> Number) -> RuntimeValue
unaryMathFunction = Function <<< unaryMathFunction'

-- The actual math functions
add :: NativeConfig
add =
  NativeConfig
    { name: FunctionName "add"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction (+))
    , functionData: internal [ numberDoc "first number", numberDoc "second number" ] { name: "sum", description: "The result of adding both arguments" }
    }

subtract :: NativeConfig
subtract =
  NativeConfig
    { name: FunctionName "subtract"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction (-))
    , functionData:
      internal [ numberDoc "first number", numberDoc "second number" ]
        { name: "difference", description: "The result of subtracting the second argument from the first" }
    }

multiply :: NativeConfig
multiply =
  NativeConfig
    { name: FunctionName "multiply"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction (*))
    , functionData:
      internal [ numberDoc "first number", numberDoc "second number" ]
        { name: "product", description: "The result of multiplying the first number by the second" }
    }

divide :: NativeConfig
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
    }

raiseToPower :: NativeConfig
raiseToPower =
  NativeConfig
    { name: FunctionName "raise to power"
    , expression: (NativeExpression binaryNumberType $ binaryMathFunction pow)
    , functionData:
      internal [ numberDoc "base", numberDoc "exponend" ]
        { name: "base^exponent", description: "The result of raising the first argument to the power of the second" }
    }

modulus :: NativeConfig
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
    }

evalSqrt :: RuntimeValue -> RuntimeValue
evalSqrt (Number num) = if num >= 0.0 then Number $ sqrt num else Null

evalSqrt _ = Null

squareRoot :: NativeConfig
squareRoot =
  NativeConfig
    { name: FunctionName "square root"
    , expression: NativeExpression unaryNumberType $ Function evalSqrt
    , functionData:
      internal
        [ { name: "radicand", description: "The number to take the square root from" } ]
        { name: "sqrt a", description: "The result of taking the square root of the number" }
    }

pred :: NativeConfig
pred =
  NativeConfig
    { name: FunctionName "predecessor"
    , expression: NativeExpression unaryNumberType $ unaryMathFunction (_ - 1.0)
    , functionData:
      internal [ { name: "number", description: "A number to get the predecessor of" } ]
        { name: "a - 1", description: "The predecessor of the input"
        }
    }

succ :: NativeConfig
succ =
  NativeConfig
    { name: FunctionName "successor"
    , expression: NativeExpression unaryNumberType $ unaryMathFunction ((+) 1.0)
    , functionData:
      internal [ { name: "number", description: "A number to get the successor  of" } ]
        { name: "a + 1", description: "The successor of the input"
        }
    }
