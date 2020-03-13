module Lunarbox.Dataflow.Expression where

import Prelude
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Lunarbox.Dataflow.Type (TVar(..), Type)

data Literal
  = LInt Int
  | LBool Boolean

derive instance literalEq :: Eq Literal

derive instance literalOrd :: Ord Literal

data NativeExpression a b
  = NativeExpression Type (a -> b)

instance eqNativeExpression :: Eq (NativeExpression a b) where
  eq (NativeExpression t _) (NativeExpression t' _) = t == t'

data Expression
  = Variable TVar
  | FunctionCall Expression Expression
  | Lambda TVar Expression
  | Literal Literal
  | Let TVar Expression Expression
  | If Expression Expression Expression
  | FixPoint Expression
  | Native (forall a b. NativeExpression a b)

derive instance expressinEq :: Eq Expression

-- HELPERS
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
