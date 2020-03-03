module Dataflow.TypeEnv (TypeEnv(..), extend) where

import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Dataflow.Type (Scheme, TVar)
import Data.Function (($))

newtype TypeEnv
  = TypeEnv (Map.Map TVar Scheme)

derive instance teNewType :: Newtype TypeEnv _

extend :: TypeEnv -> Tuple TVar Scheme -> TypeEnv
extend (TypeEnv env) (Tuple x s) = TypeEnv $ Map.insert x s env
