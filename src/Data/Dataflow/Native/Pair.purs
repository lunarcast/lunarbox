module Lunarbox.Data.Dataflow.Native.Pair
  ( pairNodes
  ) where


import Data.Bifunctor (lmap, rmap)
import Data.Symbol (SProxy)
import Data.Tuple (Tuple, curry, fst, snd, swap, uncurry)
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..))
import Lunarbox.Data.Dataflow.Runtime.Class.Describable (DProxy(..), toNativeExpression)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))

-- ALl the pair-related native nodes
pairNodes :: Array (NativeConfig)
pairNodes = [ pair, first, second, swap', curry', uncurry', lmap', rmap' ] 

--------- Nodes
pair :: NativeConfig
pair =
  NativeConfig
    { name: FunctionName "Pair"
    , expression: toNativeExpression expression 
    , functionData: internal 
        [ { name: "first", description: "The first value in the pair" }
        , { name: "second", description: "The second value in the pair" } ] 
        { name: "pair", description: "A pair containing the arguments" }
    }
  where
  expression :: DProxy (A -> B -> Tuple A B) _
  expression = DProxy Pair

first :: NativeConfig 
first =
  NativeConfig
    { name: FunctionName "first"
    , expression: toNativeExpression expression 
    , functionData: internal 
        [ { name: "pair", description: "The pair to get the first element of" } ] 
        { name: "first", description: "The first element of the given pair" }
    }
  where
  expression :: DProxy (Tuple A B -> A) _
  expression = DProxy (fst :: Tuple RuntimeValue RuntimeValue -> _)

second :: NativeConfig
second = 
  NativeConfig
    { name: FunctionName "second"
    , expression: toNativeExpression expression 
    , functionData: internal 
        [ { name: "pair", description: "The pair to get the second element of" } ] 
        { name: "second", description: "The second element of the given pair" }
    }
  where
  expression :: DProxy (Tuple A B -> B) _
  expression = DProxy (snd :: Tuple RuntimeValue RuntimeValue -> _)

swap' :: NativeConfig 
swap' =
  NativeConfig
    { name: FunctionName "swap"
    , expression: toNativeExpression expression 
    , functionData: internal 
        [ { name: "pair", description: "The pair to swap the elements of" } ] 
        { name: "swapped", description: "The pair with the same elements as the input but the other way around" }
    }
  where
  expression :: DProxy (Tuple A B -> Tuple B A) _
  expression = DProxy (swap :: Tuple RuntimeValue RuntimeValue -> _)

curry' :: NativeConfig
curry' = 
  NativeConfig
    { name: FunctionName "curry"
    , expression: toNativeExpression expression 
    , functionData: internal 
        [ { name: "uncurried", description: "A function taking a pair as it's argument" } ] 
        { name: "curried", description: "A function which takes 2 arguments" }
    }
  where
  expression :: DProxy ((Tuple A B -> C) -> A -> B -> C) _
  expression = DProxy (curry :: (Tuple RuntimeValue RuntimeValue -> RuntimeValue) -> _)

uncurry' :: NativeConfig
uncurry' = 
  NativeConfig
    { name: FunctionName "uncurry"
    , expression: toNativeExpression expression 
    , functionData: internal 
        [ { name: "curried", description: "A function taking 2 arguments" } ] 
        { name: "uncurried", description: "A function which takes a pair as the argument" }
    }
  where
  expression :: DProxy ((A -> B -> C) -> Tuple A B -> C) _
  expression = DProxy (uncurry :: (RuntimeValue -> RuntimeValue -> RuntimeValue) -> _)

lmap' :: NativeConfig
lmap' = 
  NativeConfig
    { name: FunctionName "mapLeft"
    , expression: toNativeExpression expression 
    , functionData: internal 
        [ { name: "function", description: "A function to apply to the first element of the pair" }
        , { name: "pair", description: "The pair to map the first element with" } ] 
        { name: "result", description: "A pair with the first element updated by the given function" }
    }
  where
  expression :: DProxy ((A -> B) -> Tuple A C -> Tuple B C) _
  expression = DProxy (lmap :: (RuntimeValue -> RuntimeValue) -> Tuple _ RuntimeValue -> _)

rmap' :: NativeConfig
rmap' = 
  NativeConfig
    { name: FunctionName "mapRight"
    , expression: toNativeExpression expression 
    , functionData: internal 
        [ { name: "function", description: "A function to apply to the second element of the pair" }
        , { name: "pair", description: "The pair to map the second element with" } ] 
        { name: "result", description: "A pair with the second element updated by the given function" }
    }
  where
  expression :: DProxy ((B -> C) -> Tuple A B -> Tuple A C) _
  expression = DProxy (rmap :: (RuntimeValue -> RuntimeValue) -> Tuple RuntimeValue _ -> _)

---------- SProxies
type A = SProxy "a"
type B = SProxy "b"
type C = SProxy "c"