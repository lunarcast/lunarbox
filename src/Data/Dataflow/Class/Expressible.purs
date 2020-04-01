module Lunarbox.Data.Dataflow.Class.Expressible
  ( class Expressible
  , nullExpr
  , toExpression
  , toExpressionWithLocation
  , toExpressionFromSelf
  ) where

import Prelude
import Lunarbox.Data.Dataflow.Expression (Expression(..), VarName(..))

-- Expresison which represents nothingess
nullExpr :: forall l. l -> Expression l
nullExpr l = Variable l (VarName "_")

-- Create an expression with a custom location
toExpressionWithLocation :: forall l l' a. Expressible a l' => l -> a -> Expression l
toExpressionWithLocation location = (location <$ _) <<< toExpression

-- Create an expression from something using that something as the location
toExpressionFromSelf :: forall s l'. Show s => Expressible s l' => s -> Expression s
toExpressionFromSelf x = x <$ toExpression x

-- Typeclass for stuff which can be transformed to expression
class Expressible a l | a -> l where
  toExpression :: a -> Expression l

instance expressibleString :: Expressible String Unit where
  toExpression = Variable unit <<< VarName
