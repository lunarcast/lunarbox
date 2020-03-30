module Lunarbox.Data.Dataflow.Constraint
  ( Constraint
  , ConstraintSet(..)
  ) where

import Prelude
import Data.Tuple (Tuple)
import Lunarbox.Dataflow.Type (Type)

type Constraint
  = Tuple Type Type

newtype ConstraintSet
  = ConstraintSet (Array Constraint)

derive newtype instance semigroupConstraintSet :: Semigroup ConstraintSet

derive newtype instance monoidConstraintSet :: Monoid ConstraintSet
