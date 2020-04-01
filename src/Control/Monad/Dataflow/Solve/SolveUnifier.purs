module Lunarbox.Control.Monad.Dataflow.Solve.SolveUnifier (solver, Unifier(..)) where

import Prelude
import Control.Monad.Reader (local)
import Data.Lens (set)
import Data.List ((:))
import Lunarbox.Control.Monad.Dataflow.Solve (Solve, _location)
import Lunarbox.Control.Monad.Dataflow.Solve.Unify (unify)
import Lunarbox.Data.Dataflow.Class.Substituable (Substitution, apply)
import Lunarbox.Data.Dataflow.Constraint (Constraint(..), ConstraintSet(..))

-- An unifier is basically a pair of a Substitution and a set of constraints
data Unifier l
  = Unifier Substitution (ConstraintSet l)

derive instance eqUnifier :: Eq l => Eq (Unifier l)

-- This takes an initial substitution and some constraints and solves them into a final substitution
solver :: forall l. Unifier l -> Solve l Substitution
solver (Unifier substitution (ConstraintSet ((Constraint { typeLeft, typeRight, source }) : constraints))) = do
  substitution' <- local (set _location source) $ unify typeRight typeLeft
  solver $ Unifier (substitution' <> substitution) $ ConstraintSet $ apply substitution' constraints

solver (Unifier substitution _) = pure substitution
