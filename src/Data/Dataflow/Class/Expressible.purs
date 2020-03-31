module Lunarbox.Data.Dataflow.Class.Expressible
  ( class Expressible
  , nullExpr
  , toExpression
  ) where

import Prelude
import Lunarbox.Data.Dataflow.Expression (Expression(..), VarName(..))

nullExpr :: forall l. l -> Expression l
nullExpr l = Variable l (VarName "__nothing")

class Expressible a l | a -> l where
  toExpression :: a -> Expression l

instance expressibleString :: Expressible String Unit where
  toExpression = Variable unit <<< VarName
