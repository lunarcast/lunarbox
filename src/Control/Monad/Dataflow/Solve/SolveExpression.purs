module Lunarbox.Control.Monad.Dataflow.Solve.SolveExpression
  ( solveExpression
  ) where

import Prelude
import Data.Either (Either)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Lunarbox.Control.Monad.Dataflow.Infer (InferEnv(..), InferOutput(..), runInfer)
import Lunarbox.Control.Monad.Dataflow.Infer.InferExpression (infer)
import Lunarbox.Control.Monad.Dataflow.Solve (SolveContext(..), runSolve)
import Lunarbox.Control.Monad.Dataflow.Solve.SolveConstraintSet (solve)
import Lunarbox.Data.Dataflow.Class.Substituable (apply)
import Lunarbox.Data.Dataflow.Expression (Expression, getLocation)
import Lunarbox.Data.Dataflow.Type (Type)
import Lunarbox.Data.Dataflow.TypeError (TypeError)

-- Takes an expression and returns a typeMap
solveExpression :: forall l. Ord l => Expression l -> Either (TypeError l) (Map.Map l Type)
solveExpression expression = do
  let
    location = getLocation expression

    inferEnv =
      InferEnv
        { typeEnv: mempty
        , location
        }

    solveContext = SolveContext { location }
  Tuple _ (InferOutput { typeMap, constraints }) <- runInfer inferEnv $ infer expression
  substitution <- runSolve solveContext $ solve constraints
  pure $ (apply substitution <$> typeMap)
