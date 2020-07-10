module Lunarbox.Data.Dataflow.Runtime.TermEnvironment
  ( TermEnvironment(..)
  , Term(..)
  , lookup
  , insert
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Default (class Default)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Lunarbox.Data.Dataflow.Expression (Expression)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..))

-- | We use this to be able to store closures
data Term l
  = Closure (TermEnvironment l) String (Expression l)
  | Term RuntimeValue
  | Code (TermEnvironment l) (Expression l)

derive instance eqTerm :: Eq l => Eq (Term l)

instance defTerm :: Default (Term l) where
  def = Term Null

instance encodeJsonTerm :: EncodeJson l => EncodeJson (Term l) where
  encodeJson (Term val) = encodeJson val
  encodeJson _ = encodeJson Null

instance decodeJsonTerm :: DecodeJson l => DecodeJson (Term l) where
  decodeJson = map Term <<< decodeJson

-- Structure used to store the value of different variables
newtype TermEnvironment l
  = TermEnvironment (Map.Map String (Term l))

derive instance eqTermEnvironment :: Eq l => Eq (TermEnvironment l)

derive instance newtypeTermEnvironment :: Newtype (TermEnvironment l) _

derive newtype instance semigroupTermEnvironment :: Semigroup l => Semigroup (TermEnvironment l)

derive newtype instance monoidTermEnvironment :: Monoid (TermEnvironment l)

derive newtype instance encodeJsonTermEnvironment :: EncodeJson l => EncodeJson (TermEnvironment l)

derive newtype instance decodeJsonTermEnvironment :: DecodeJson l => DecodeJson (TermEnvironment l)

-- Same as Map.lookup but returns Null in case the value cannot be found
lookup :: forall l. String -> TermEnvironment l -> Term l
lookup key = fromMaybe (Term Null) <<< Map.lookup key <<< unwrap

-- Wrapper around Map.insert
insert :: forall l. String -> Term l -> TermEnvironment l -> TermEnvironment l
insert key value (TermEnvironment env) = TermEnvironment $ Map.insert key value env
