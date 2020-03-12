module Lunarbox.Dataflow.Expression where

import Prelude
import Lunarbox.Dataflow.Type (TVar, Type)

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
