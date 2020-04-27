module Lunarbox.Data.Dataflow.Runtime.ValueMap
  ( ValueMap(..)
  ) where

import Prelude
import Data.Map as Map
import Data.Newtype (class Newtype)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)

-- A map holding the runtime values of different locations
newtype ValueMap v l
  = ValueMap (Map.Map l (RuntimeValue v))

derive instance eqValueMap :: (Eq l, Eq v) => Eq (ValueMap v l)

derive instance newtypeValueMap :: Newtype (ValueMap v l) _

derive newtype instance semigroupValueMap :: Ord l => Semigroup (ValueMap v l)

derive newtype instance monoidValueMap :: Ord l => Monoid (ValueMap v l)
