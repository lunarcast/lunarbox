module Lunarbox.Data.Editor.FunctionUi
  ( FunctionUiActions
  , FunctionUiInputs
  , FunctionUi
  ) where

import Data.Maybe (Maybe)
import Halogen.HTML (HTML)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)

type FunctionUiActions a
  = { setValue :: RuntimeValue -> Maybe a
    }

type FunctionUiInputs
  = { value :: RuntimeValue
    }

-- Functions can have custom ui under them
type FunctionUi h a
  = FunctionUiInputs -> FunctionUiActions a -> HTML h a
