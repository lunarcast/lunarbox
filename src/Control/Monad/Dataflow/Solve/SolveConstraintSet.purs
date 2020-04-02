module Lunarbox.Control.Monad.Dataflow.Solve.SolveConstraintSet
  ( solve
  ) where

import Prelude
import Control.Monad.Reader (local)
import Data.Lens (set)
import Data.List ((:))
import Lunarbox.Control.Monad.Dataflow.Solve (Solve, _location)
import Lunarbox.Control.Monad.Dataflow.Solve.Unify (unify)
import Lunarbox.Data.Dataflow.Class.Substituable (Substitution, apply)
import Lunarbox.Data.Dataflow.Constraint (Constraint(..), ConstraintSet(..))

-- internal version solve which takes an Unifier
solve' :: forall l. Substitution -> ConstraintSet l -> Solve l Substitution
solve' substitution (ConstraintSet ((Constraint { typeLeft, typeRight, source }) : constraints)) = do
  substitution' <- local (set _location source) $ unify typeRight typeLeft
  solve' (substitution' <> substitution) $ ConstraintSet $ apply substitution' constraints

solve' substitution _ = pure substitution

-- This takes some constraints and solves them into a final substitution
solve :: forall l. ConstraintSet l -> Solve l Substitution
solve = solve' mempty
