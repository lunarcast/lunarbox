module Lunarbox.Data.Dataflow.Runtime
  ( RuntimeValue
  ) where

import Prelude

data RuntimeValue
  = Number Number
  | String String
  | Function RuntimeValue RuntimeValue

derive instance eqRuntimeValue :: Eq RuntimeValue
