module Lunarbox.Data.Dataflow.Runtime
  ( RuntimeValue(..)
  , binaryFunction
  , toBoolean
  , _Number
  , _String
  , _Function
  ) where

import Prelude
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))

-- Representations of all possible runtime values
data RuntimeValue
  = Number Number
  | String String
  | Bool Boolean
  | Null
  | Function (RuntimeValue -> RuntimeValue)

instance showRuntimeValue :: Show RuntimeValue where
  show = case _ of
    Null -> "null"
    Bool value -> show value
    Number value -> show value
    String value -> show value
    Function value -> "Function"

instance eqRuntimeValue :: Eq RuntimeValue where
  eq (Number n) (Number n') = n == n'
  eq (String s) (String s') = s == s'
  eq (Bool v) (Bool v') = v == v'
  eq Null Null = true
  eq _ _ = false

-- helper to ease the creation of binary functions
binaryFunction :: (RuntimeValue -> RuntimeValue -> RuntimeValue) -> RuntimeValue
binaryFunction f = Function $ Function <<< f

-- Turns any runtime value to a boolean
toBoolean :: RuntimeValue -> Boolean
toBoolean value
  | value == Bool true = true
  | otherwise = false

-- Lenses
_Number :: Prism' RuntimeValue Number
_Number =
  prism' Number case _ of
    Number c -> Just c
    _ -> Nothing

_String :: Prism' RuntimeValue String
_String =
  prism' String case _ of
    String c -> Just c
    _ -> Nothing

_Function :: Prism' RuntimeValue (RuntimeValue -> RuntimeValue)
_Function =
  prism' Function case _ of
    Function c -> Just c
    _ -> Nothing
