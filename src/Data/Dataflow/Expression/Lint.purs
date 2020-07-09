module Lunarbox.Data.Dataflow.Expression.Lint where

import Prelude
import Data.Array as Array
import Lunarbox.Data.Dataflow.Expression (Expression(..), VarName, foldExpression)

data LintError l
  = UnusedDeclaration VarName l
  | UnsaturatedFunction l l

-- | Collect linting errors inside an expression
lint :: forall l. Expression l -> Array (LintError l)
lint = foldExpression go
  where
  go (Let location name _ body)
    | Array.null (references name body) =
      [ UnusedDeclaration name location
      ]

  go (FunctionCall location _ (TypedHole argLocation)) =
    [ UnsaturatedFunction location argLocation
    ]

  go a = []

-- | Get all the places a variable is referenced in
references :: forall l. VarName -> Expression l -> Array l
references target = foldExpression go
  where
  go (Variable location name)
    | target == name = [ location ]

  go _ = []
