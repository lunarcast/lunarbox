module Lunarbox.Data.Set (NativeSet, toNative) where

import Prelude
import Data.Set (Set)
import Data.Set as Set

foreign import data NativeSet :: Type -> Type

foreign import arrayToSet :: forall a. Array a -> NativeSet a

-- | Convert a purs set to a js one.
toNative :: Set ~> NativeSet
toNative = arrayToSet <<< Set.toUnfoldable
