module Lunarbox.Data.Editor.FunctionUi
  ( FunctionUiActions
  , FunctionUiInputs
  , FunctionUi
  ) where

import Data.Maybe (Maybe)
import Halogen.HTML (ComponentHTML)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)

type FunctionUiActions a
  = { setValue :: RuntimeValue -> Maybe a
    }

type FunctionUiInputs
  = { value :: RuntimeValue
    }

-- Functions can have custom ui under them
type FunctionUi a s m
  = FunctionUiInputs -> FunctionUiActions a -> ComponentHTML a s m
