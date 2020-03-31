module Lunarbox.Data.Dataflow.TypeError
  ( TypeError(..)
  ) where

import Prelude
import Lunarbox.Data.Dataflow.Expression (VarName(..))
import Lunarbox.Data.Dataflow.Type (TVarName, Type)

-- Type for all type errors
-- At the moment there are 4 possible type errors:
-- 1) Trying to use a type t1 when a type t2 is expected => TypeMissmatch
-- 2) Trying to use a function where a type is expected => DifferentLength
-- 3) Using a type which contains itself => RecursiveType
-- 4) Trying to access a variable which isn't in scope => UnboundVariable
-- All the errors hold a "l" argument which represents the location where the error occured
data TypeError l
  = TypeMissmatch Type Type l
  | DifferentLength (Array Type) (Array Type) l
  | RecursiveType TVarName Type l
  | UnboundVariable VarName l

instance showTypeError :: Show (TypeError l) where
  show (TypeMissmatch t1 t2 _) = "Could not match type " <> show t1 <> " with type " <> show t2
  show (DifferentLength t1 t2 _) = "Could not match types " <> show t1 <> " with types " <> show t2 <> " because the lengths are different"
  show (RecursiveType v t _) = "Type " <> show t <> " contains a reference to itself"
  show (UnboundVariable v _) = "Variable " <> show v <> " is not in scope"
