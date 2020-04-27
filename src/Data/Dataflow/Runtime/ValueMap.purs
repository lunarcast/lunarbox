module Lunarbox.Data.Dataflow.Runtime.ValueMap
  ( ValueMap(..)
  ) where

import Prelude
import Data.Default (class Default)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)

-- A map holding the runtime values of different locations
newtype ValueMap l
  = ValueMap (Map.Map l RuntimeValue)

derive instance eqValueMap :: Eq l => Eq (ValueMap l)

derive instance newtypeValueMap :: Newtype (ValueMap l) _

derive newtype instance semigroupValueMap :: Ord l => Semigroup (ValueMap l)

derive newtype instance monoidValueMap :: Ord l => Monoid (ValueMap l)

derive newtype instance showValueMap :: Show l => Show (ValueMap l)

instance defaultValueMap :: Default (ValueMap l) where
  def = ValueMap $ Map.empty
