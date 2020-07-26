module Lunarbox.Data.Dataflow.Native.Predicate (predicateNodes) where

import Prelude
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (createTypeVariable, typeBool, typeFunction)
import Lunarbox.Data.Editor.FunctionData (PinDoc, internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))

-- All the nodes which test stuff returning booleans
predicateNodes :: Array (NativeConfig)
predicateNodes = [ equal, smallerThan, greaterThan, greaterOrEqual, smallerOrEqual ]

-- Type for a function which akes 2 values of the same type and returns a boolean
typeBinaryCompare :: Scheme
typeBinaryCompare = Forall [ a ] $ typeFunction typeA $ typeFunction typeA typeBool
  where
  Tuple a typeA = createTypeVariable "t0"

-- Helper to generate a config for a predicate  of type typeBinaryCompare
createBinaryCompare :: String -> PinDoc -> (RuntimeValue -> RuntimeValue -> Boolean) -> NativeConfig
createBinaryCompare name output predicate =
  NativeConfig
    { name: FunctionName name
    , expression: NativeExpression typeBinaryCompare $ binaryFunction ((Bool <<< _) <<< predicate)
    , functionData: internal [ { name: "first value", description: "Any value" }, { name: "second value", description: "Any value" } ] output
    }

-- The actual predicates
equal :: NativeConfig
equal = createBinaryCompare "are equal" { name: "a == b", description: "Return true only if both values are equal" } (==)

smallerThan :: NativeConfig
smallerThan =
  createBinaryCompare "smaller than"
    { name: "a < b"
    , description: "Compares both values and returns true if the first one is greater than the second"
    }
    (<)

greaterThan :: NativeConfig
greaterThan =
  createBinaryCompare "greater than"
    { name: "a > b"
    , description: "Compares both values and returns true if the first one is greater than the second"
    }
    (>)

greaterOrEqual :: NativeConfig
greaterOrEqual =
  createBinaryCompare "greater or equal"
    { name: "a >= b"
    , description: "Compares both values and returns true if the first one is greater or equal than the second"
    }
    (>=)

smallerOrEqual :: NativeConfig
smallerOrEqual =
  createBinaryCompare "smaller or equal"
    { name: "a <= b"
    , description: "Compares both values and returns true if the first one is smaller or equal than the second"
    }
    (<=)
