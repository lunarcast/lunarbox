module Lunarbox.Data.Dataflow.Runtime
  ( RuntimeValue(..)
  , TermEnvironment(..)
  , binaryFunction
  , lookup
  , _Number
  , _String
  , _Function
  ) where

import Prelude
import Data.Lens (Prism', prism')
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)

-- Structure used to store the value of different variables
newtype TermEnvironment c
  = TermEnvironment (Map.Map String (RuntimeValue c))

derive instance eqTermEnvironment :: Eq c => Eq (TermEnvironment c)

derive instance newtypeTermEnvironment :: Newtype (TermEnvironment c) _

-- Same as Map.lookup but returns Null in case the value cannot be found
lookup :: forall c. String -> TermEnvironment c -> RuntimeValue c
lookup key = fromMaybe Null <<< Map.lookup key <<< unwrap

-- Representations of all possible runtime values
data RuntimeValue c
  = Number Number
  | String String
  | Bool Boolean
  | Null
  -- Note: this should only be used for native expressions. For interpreted expressions use 
  | Function (RuntimeValue c -> RuntimeValue c)
  | Closure String c (TermEnvironment c)

instance eqRuntimeValue :: Eq c => Eq (RuntimeValue c) where
  eq (Number n) (Number n') = n == n'
  eq (String s) (String s') = s == s'
  eq Null Null = true
  eq _ _ = false

-- helper to ease the creation of binary functions
binaryFunction :: forall c. (RuntimeValue c -> RuntimeValue c -> RuntimeValue c) -> RuntimeValue c
binaryFunction f = Function $ Function <<< f

-- Lenses
_Number :: forall c. Prism' (RuntimeValue c) Number
_Number =
  prism' Number case _ of
    Number c -> Just c
    _ -> Nothing

_String :: forall c. Prism' (RuntimeValue c) String
_String =
  prism' String case _ of
    String c -> Just c
    _ -> Nothing

_Function :: forall c. Prism' (RuntimeValue c) (RuntimeValue c -> RuntimeValue c)
_Function =
  prism' Function case _ of
    Function c -> Just c
    _ -> Nothing
