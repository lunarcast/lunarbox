module Lunarbox.Data.Dataflow.Native.Math
  ( mathNodes
  ) where

import Prelude
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime.Class.Describable (toNativeExpression)
import Lunarbox.Data.Editor.FunctionData (PinDoc, internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Math (pow, sqrt, (%))

-- ALl the math native nodes
mathNodes :: Array (NativeConfig)
mathNodes = [ add, subtract, multiply, divide, raiseToPower, modulus, squareRoot, pred, succ ]

-- Documentation for an numeral value
numberDoc :: String -> PinDoc
numberDoc = { name: _, description: "Any numeric value" }

-- The actual math functions
add :: NativeConfig
add =
  NativeConfig
    { name: FunctionName "add"
    , expression: toNativeExpression ((+) :: Number -> _)
    , functionData: internal [ numberDoc "first number", numberDoc "second number" ] { name: "sum", description: "The result of adding both arguments" }
    }

subtract :: NativeConfig
subtract =
  NativeConfig
    { name: FunctionName "subtract"
    , expression: toNativeExpression ((-) :: Number -> _)
    , functionData:
      internal [ numberDoc "first number", numberDoc "second number" ]
        { name: "difference", description: "The result of subtracting the second argument from the first" }
    }

multiply :: NativeConfig
multiply =
  NativeConfig
    { name: FunctionName "multiply"
    , expression: toNativeExpression ((*) :: Number -> _)
    , functionData:
      internal [ numberDoc "first number", numberDoc "second number" ]
        { name: "product", description: "The result of multiplying the first number by the second" }
    }

divide :: NativeConfig
divide =
  NativeConfig
    { name: FunctionName "divide"
    , expression: toNativeExpression ((/) :: Number -> _)
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
    , expression: toNativeExpression pow
    , functionData:
      internal [ numberDoc "base", numberDoc "exponend" ]
        { name: "base^exponent", description: "The result of raising the first argument to the power of the second" }
    }

modulus :: NativeConfig
modulus =
  NativeConfig
    { name: FunctionName "modulus"
    , expression: toNativeExpression (%)
    , functionData:
      internal
        [ { name: "left side", description: "The number to take the modulus from" }
        , { name: "right side", description: "The number to divide the first input by and find the remainder" }
        ]
        { name: "a % b", description: "The remainder of dividing the first number to the second" }
    }

squareRoot :: NativeConfig
squareRoot =
  NativeConfig
    { name: FunctionName "square root"
    , expression: toNativeExpression sqrt
    , functionData:
      internal
        [ { name: "radicand", description: "The number to take the square root from" } ]
        { name: "sqrt a", description: "The result of taking the square root of the number" }
    }

pred :: NativeConfig
pred =
  NativeConfig
    { name: FunctionName "predecessor"
    , expression: toNativeExpression (_ - 1.0)
    , functionData:
      internal [ { name: "number", description: "A number to get the predecessor of" } ]
        { name: "a - 1", description: "The predecessor of the input"
        }
    }

succ :: NativeConfig
succ =
  NativeConfig
    { name: FunctionName "successor"
    , expression: toNativeExpression (1.0 + _)
    , functionData:
      internal [ { name: "number", description: "A number to get the successor  of" } ]
        { name: "a + 1", description: "The successor of the input"
        }
    }
