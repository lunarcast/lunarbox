module Lunarbox.Data.Dataflow.Native.Array (arrayNodes) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (createTypeVariable, typeArray, typeFunction)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))

-- List will all the native array nodes
arrayNodes :: forall a s m. Array (NativeConfig a s m)
arrayNodes = [ emptyArray, cons, map' ]

-- A constant equal to an array with 0 elements
typeEmptyArray :: Scheme
typeEmptyArray = Forall [ a ] $ typeArray typeA
  where
  Tuple a typeA = createTypeVariable "t0"

emptyArray :: forall a s m. NativeConfig a s m
emptyArray =
  NativeConfig
    { name: FunctionName "empty array"
    , expression: (NativeExpression typeEmptyArray $ NArray [])
    , functionData: internal [] { name: "array" }
    , component: Nothing
    }

-- Given a and [a] returns an array having a as the first element and everything else afte that
typeCons :: Scheme
typeCons = Forall [ a ] $ typeFunction typeA $ typeFunction (typeArray typeA) (typeArray typeA)
  where
  Tuple a typeA = createTypeVariable "t0"

evalCons :: RuntimeValue -> RuntimeValue -> RuntimeValue
evalCons element (NArray array) = NArray $ pure element <> array

evalCons _ _ = Null

cons :: forall a s m. NativeConfig a s m
cons =
  NativeConfig
    { name: FunctionName "cons"
    , expression: (NativeExpression typeCons $ binaryFunction evalCons)
    , functionData: internal [ { name: "element" }, { name: "array" } ] { name: "array" }
    , component: Nothing
    }

-- Mapping over arrays
typeMap :: Scheme
typeMap = Forall [ a, b ] $ typeFunction (typeFunction typeA typeB) $ typeFunction (typeArray typeA) $ typeArray typeB
  where
  Tuple a typeA = createTypeVariable "t0"

  Tuple b typeB = createTypeVariable "t1"

evalMap :: RuntimeValue -> RuntimeValue -> RuntimeValue
evalMap (Function mapper) (NArray array) = NArray $ mapper <$> array

evalMap _ _ = Null

map' :: forall a s m. NativeConfig a s m
map' =
  NativeConfig
    { name: FunctionName "map array"
    , expression: (NativeExpression typeMap $ binaryFunction evalMap)
    , functionData: internal [ { name: "mapper" }, { name: "array" } ] { name: "array" }
    , component: Nothing
    }
