module Lunarbox.Data.Dataflow.Runtime.Class.Runnable where

import Prelude
import Data.Default (class Default, def)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (isNaN)
import Data.Traversable (traverse)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..))
import Math (floor)

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
else instance corunnableArrow' :: (Runnable a, Corunnable b, Default b) => Corunnable (a -> b) where
  fromRuntime (Function f) = Just $ fromMaybe def <<< fromRuntime <<< f <<< toRuntime
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

instance runnableMaybe :: Runnable a => Runnable (Maybe a) where
  toRuntime = maybe Null toRuntime

-- | Run a non runtime function over a runtime value
overRuntimeValue :: forall a b. Runnable b => Corunnable a => (a -> b) -> RuntimeValue -> RuntimeValue
overRuntimeValue func = toRuntime <<< map func <<< fromRuntime
