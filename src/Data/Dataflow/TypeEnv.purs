module Lunarbox.Data.Dataflow.TypeEnv
  ( TypeEnv(..)
  , singleton
  ) where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Lunarbox.Data.Dataflow.Expression (VarName)
import Lunarbox.Data.Dataflow.Scheme (Scheme)

newtype TypeEnv
  = TypeEnv (Map VarName Scheme)

derive instance newTypeTypeEnv :: Newtype TypeEnv _

derive newtype instance semigroupTypeEnv :: Semigroup TypeEnv

derive newtype instance monoidTypeEnv :: Monoid TypeEnv

singleton :: VarName -> Scheme -> TypeEnv
singleton name scheme = TypeEnv $ Map.singleton name scheme
