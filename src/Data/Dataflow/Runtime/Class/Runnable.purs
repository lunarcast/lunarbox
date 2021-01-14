module Lunarbox.Data.Dataflow.Runtime.Class.Runnable where

import Prelude

import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (isNaN)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..))
import Math (floor)

--------- Helper classes
class RuntimeDefault a where
  runtimeDefault :: a

instance runtimeDefaultRV :: RuntimeDefault RuntimeValue where
  runtimeDefault = Null
else instance runtimeDefaultArrow :: RuntimeDefault a => RuntimeDefault (anything -> a) where
  runtimeDefault _ = runtimeDefault

---------- Runnable and Corrunable
class Runnable a where
  toRuntime :: a -> RuntimeValue

class Corunnable a where
  fromRuntime :: RuntimeValue -> Maybe a

instance runnableInt :: Runnable Int where
  toRuntime = Number <<< toNumber

instance coRunnableInt :: Corunnable Int where
  fromRuntime (Number inner) = fromNumber inner
  fromRuntime _ = Nothing

instance runnableNumber :: Runnable Number where
  toRuntime a
    | isNaN a = Null
    | otherwise = Number $ floorAt 1000.0 a
      where
      floorAt at x = floor (x * at) / at

instance corunnableNumber :: Corunnable Number where
  fromRuntime (Number inner) = Just inner
  fromRuntime _ = Nothing

instance runnableBool :: Runnable Boolean where
  toRuntime = Bool

instance corunnableBool :: Corunnable Boolean where
  fromRuntime (Bool a) = Just a
  fromRuntime _ = Nothing

instance runnableRuntimeValue :: Runnable RuntimeValue where
  toRuntime = identity

instance corunnaleRuntimeValue :: Corunnable RuntimeValue where
  fromRuntime = Just

instance runnableArrow :: (Corunnable a, Runnable b) => Runnable (a -> b) where
  toRuntime f = Function go
    where
    go a = case fromRuntime a of
      Just inner -> toRuntime $ f inner
      Nothing -> Null

instance corunnableArrow :: (Runnable a, Corunnable b) => Corunnable (a -> Maybe b) where
  fromRuntime (Function f) = Just $ fromRuntime <<< f <<< toRuntime
  fromRuntime _ = Nothing
else instance corunnableArrow' :: Runnable a => Corunnable (a -> RuntimeValue) where
  fromRuntime (Function f) = Just $ fromMaybe Null <<< fromRuntime <<< f <<< toRuntime
  fromRuntime _ = Nothing
else instance corunnableArrow'' :: (Runnable a, Corunnable b, RuntimeDefault b) => Corunnable (a -> b) where
  fromRuntime (Function f) = Just $ fromMaybe runtimeDefault <<< fromRuntime <<< f <<< toRuntime
  fromRuntime _ = Nothing

instance runnableString :: Runnable String where
  toRuntime = String

instance corunnableString :: Corunnable String where
  fromRuntime (String a) = Just a
  fromRuntime _ = Nothing

instance runnableArray :: Runnable a => Runnable (Array a) where
  toRuntime = NArray <<< map toRuntime

instance corunnableArray :: Corunnable a => Corunnable (Array a) where
  fromRuntime (NArray arr) = traverse fromRuntime arr
  fromRuntime _ = Nothing

instance runnableTuple :: (Runnable a, Runnable b) => Runnable (Tuple a b) where
  toRuntime (Tuple a b) = Pair (toRuntime a) (toRuntime b)

instance corunnableTuple :: (Corunnable a, Corunnable b) => Corunnable (Tuple a b) where
  fromRuntime (Pair a b) = Tuple <$> fromRuntime a <*> fromRuntime b
  fromRuntime _ = Nothing

-- | Run a non runtime function over a runtime value
overRuntimeValue :: forall a b. Runnable b => Corunnable a => (a -> b) -> RuntimeValue -> RuntimeValue
overRuntimeValue func = maybe Null toRuntime <<< map func <<< fromRuntime
