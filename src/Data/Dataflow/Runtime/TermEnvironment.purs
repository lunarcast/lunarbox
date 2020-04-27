module Lunarbox.Data.Dataflow.Runtime.TermEnvironment
  ( TermEnvironment(..)
  , lookup
  , insert
  ) where

import Prelude
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..))

-- Structure used to store the value of different variables
newtype TermEnvironment
  = TermEnvironment (Map.Map String RuntimeValue)

derive instance eqTermEnvironment :: Eq TermEnvironment

derive instance newtypeTermEnvironment :: Newtype TermEnvironment _

derive newtype instance semigroupTermEnvironment :: Semigroup TermEnvironment

derive newtype instance monoidTermEnvironment :: Monoid TermEnvironment

-- Same as Map.lookup but returns Null in case the value cannot be found
lookup :: String -> TermEnvironment -> RuntimeValue
lookup key = fromMaybe Null <<< Map.lookup key <<< unwrap

-- Wrapper around Map.insert
insert :: String -> RuntimeValue -> TermEnvironment -> TermEnvironment
insert key value (TermEnvironment env) = TermEnvironment $ Map.insert key value env
