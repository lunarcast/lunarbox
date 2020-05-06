module Lunarbox.Data.Dataflow.Native.Predicate (predicateNodes) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (createTypeVariable, typeBool, typeFunction)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))

-- All the nodes which test stuff returning booleans
predicateNodes :: forall a s m. Array (NativeConfig a s m)
predicateNodes = [ equal, smallerThan, greaterThan, greaterOrEqual, smallerOrEqual ]

-- Type for a function which akes 2 values of the same type and returns a boolean
typeBinaryCompare :: Scheme
typeBinaryCompare = Forall [ a ] $ typeFunction typeA $ typeFunction typeA typeBool
  where
  Tuple a typeA = createTypeVariable "t0"

-- Helper to generate a config for a predicate  of type typeBinaryCompare
createBinaryCompare :: forall a s m. String -> String -> (RuntimeValue -> RuntimeValue -> Boolean) -> NativeConfig a s m
createBinaryCompare name output predicate =
  NativeConfig
    { name: FunctionName name
    , expression: NativeExpression typeBinaryCompare $ binaryFunction ((Bool <<< _) <<< predicate)
    , functionData: internal [ { name: "first value" }, { name: "second value" } ] { name: output }
    , component: Nothing
    }

-- The actual predicates
equal :: forall a s m. NativeConfig a s m
equal = createBinaryCompare "are equal" "a == b" (==)

smallerThan :: forall a s m. NativeConfig a s m
smallerThan = createBinaryCompare "smaller than" "a < b" (<)

greaterThan :: forall a s m. NativeConfig a s m
greaterThan = createBinaryCompare "greater than" "a > b" (>)

greaterOrEqual :: forall a s m. NativeConfig a s m
greaterOrEqual = createBinaryCompare "greater or equal" "a >= b" (>=)

smallerOrEqual :: forall a s m. NativeConfig a s m
smallerOrEqual = createBinaryCompare "smaller or equal" "a <= b" (<=)
