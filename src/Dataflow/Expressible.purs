module Lunarbox.Dataflow.Expressible where

import Prelude
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Lunarbox.Dataflow.Expression (Expression(..))
import Lunarbox.Dataflow.Type (TVar(..))

nullExpr :: Expression
nullExpr = Variable $ TV $ "__nothing"

class Expressible a where
  toExpression :: a -> Expression

instance expressibleString :: Expressible String where
  toExpression = Variable <<< TV

instance expressibleMaybe :: Expressible a => Expressible (Maybe a) where
  toExpression = fromMaybe nullExpr <<< (toExpression <$> _)

instance expressibleExpression :: Expressible Expression where
  toExpression = identity

newtypeToExpression :: forall a t. Newtype a t => Expressible t => a -> Expression
newtypeToExpression = toExpression <<< unwrap
