module Lunarbox.Data.Dataflow.Expression.Lint where

import Prelude
import Data.Array as Array
import Lunarbox.Data.Dataflow.Expression (Expression(..), VarName, foldExpression)

-- | Basically warnings the user gets for imrpoving code clarity
data LintError l
  = UnusedDeclaration l VarName
  | UnsaturatedFunction l l

-- | Get the location a linting error came from
getLocation :: forall l. LintError l -> l
getLocation (UnusedDeclaration location _) = location

getLocation (UnsaturatedFunction location _) = location

-- | Collect linting errors inside an expression
lint :: forall l. Expression l -> Array (LintError l)
lint = foldExpression go
  where
  go (Let location name _ body)
    | Array.null (references name body) =
      [ UnusedDeclaration location name
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
