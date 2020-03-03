module Dataflow.Expression (Expression(..), Literal(..)) where

import Prelude
import Dataflow.Type (TVar)

data Literal
  = LInt Int
  | LBool Boolean

derive instance literalEq :: Eq Literal

derive instance literalOrd :: Ord Literal

data Expression
  = Variable TVar
  | FunctionCall Expression Expression
  | Lambda TVar Expression
  | Literal Literal
  | Let TVar Expression Expression
  | If Expression Expression Expression
  | FixPoint Expression

derive instance expressinEq :: Eq Expression
