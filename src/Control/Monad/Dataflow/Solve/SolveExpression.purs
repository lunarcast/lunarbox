module Lunarbox.Control.Monad.Dataflow.Solve.SolveExpression
  ( solveExpression
  , printTypeMap
  ) where

import Prelude
import Data.Array (foldr)
import Data.Array as Array
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Lunarbox.Capability.Editor.Type (prettify)
import Lunarbox.Control.Monad.Dataflow.Infer (InferEnv(..), InferOutput(..), runInfer)
import Lunarbox.Control.Monad.Dataflow.Infer.InferExpression (infer)
import Lunarbox.Control.Monad.Dataflow.Solve (SolveContext(..), SolveState(..), runSolve)
import Lunarbox.Control.Monad.Dataflow.Solve.SolveConstraintSet (solve)
import Lunarbox.Data.Dataflow.Class.Substituable (apply)
import Lunarbox.Data.Dataflow.Expression (Expression, getLocation)
import Lunarbox.Data.Dataflow.Type (Type)

-- Takes an expression and returns a typeMap
solveExpression :: forall l. Ord l => Show l => Expression l -> Tuple (Map.Map l Type) (SolveState l)
solveExpression expression = Tuple (apply substitution <$> typeMap) (SolveState { errors } <> otherState)
  where
  location = getLocation expression

  inferEnv =
    InferEnv
      { typeEnv: mempty
      , location
      }

  solveContext = SolveContext { location }

  (Tuple _ (InferOutput { typeMap, constraints, errors })) = runInfer inferEnv $ infer expression

  (Tuple substitution otherState) = runSolve solveContext $ solve constraints

-- helper to print a typemap
printTypeMap :: forall l. Show l => Ord l => Map.Map l Type -> String
printTypeMap =
  foldr (\(Tuple location type') result -> result <> "\n" <> show location <> " = " <> show (prettify $ type')) ""
    <<< Array.sortBy (\(Tuple _ a) (Tuple _ b) -> compare (show a) $ show b)
    <<< Map.toUnfoldable
