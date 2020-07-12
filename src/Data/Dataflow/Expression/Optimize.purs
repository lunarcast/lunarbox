module Lunarbox.Data.Dataflow.Expression.Optimize where

import Prelude
import Data.Array as Array
import Lunarbox.Data.Dataflow.Expression (Expression(..), everywhereOnExpression, mapExpression, references, wrap)

-- | Inline variales only used once an expression
inline :: forall l. Eq l => Expression l -> Expression l
inline = mapExpression go
  where
  go expr@(Let location name value body) = case references name body of
    [ ref ] -> wrap location $ everywhereOnExpression (const true) go' body
      where
      go' (Variable location' name')
        | name' == name && location' == ref = wrap location' value

      go' a = a
    _ -> expr

  go expr = expr

-- | Remove unused stuff
dce :: forall l. Eq l => Expression l -> Expression l
dce = mapExpression go
  where
  go (Let location name _ body)
    | Array.null (references name body) = wrap location body

  go expr = expr
