module Lunarbox.Data.Dataflow.Runtime.TermEnvironment
  ( TermEnvironment(..)
  ) where

import Prelude
import Data.Map as Map
import Data.Newtype (class Newtype)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)

newtype TermEnvironment
  = TermEnvironment (Map.Map String RuntimeValue)

derive instance eqTermEnvironment :: Eq TermEnvironment

derive instance newtypeTermEnvironment :: Newtype TermEnvironment _
