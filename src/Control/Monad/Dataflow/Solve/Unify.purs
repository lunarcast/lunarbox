module Lunarbox.Control.Monad.Dataflow.Solve.Unify where

import Prelude
import Lunarbox.Data.Dataflow.Class.Substituable (Substitution(..))
import Lunarbox.Data.Dataflow.Constraint (ConstraintSet(..))

-- An unifier is basically a pair of a Substitution and a set of constraints
data Unifier l
  = Unifier Substitution (ConstraintSet l)

derive instance eqUnifier :: Eq l => Eq (Unifier l)
