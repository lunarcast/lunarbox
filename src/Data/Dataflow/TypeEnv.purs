module Lunarbox.Data.Dataflow.TypeEnv (TypeEnv(..)) where

import Prelude
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Dataflow.Expression (VariableName)
import Lunarbox.Data.Dataflow.Scheme (Scheme)

newtype TypeEnv
  = TypeEnv (Map VariableName Scheme)

derive instance newTypeTypeEnv :: Newtype TypeEnv _

derive newtype instance indexTypeEnv :: Index TypeEnv

derive newtype instance atTypeEnv :: At TypeEnv
