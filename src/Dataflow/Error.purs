module Dataflow.Error (TypeError(..)) where

import Prelude
import Dataflow.Type (Type, TVar)

data TypeError
  = TypeMissmatch Type Type
  | DifferentLength (Array Type) (Array Type)
  | RecursiveType TVar Type
  | UnboundVariable TVar

instance showTypeError :: Show TypeError where
  show (TypeMissmatch t1 t2) = "Could not match type " <> show t1 <> " with type " <> show t2
  show (DifferentLength t1 t2) = "Could not match types " <> show t1 <> " with types " <> show t2 <> " because the lengths are different"
  show (RecursiveType v t) = "Type " <> show t <> " contains a reference to itself"
  show (UnboundVariable v) = "Variable " <> show v <> " is not in scope"
