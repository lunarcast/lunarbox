module Lunarbox.Data.Vector where

import Data.Typelevel.Num (D2)
import Data.Vec (Vec)

-- Shorthand for vectors with 2 elements
-- Basically with this package we specify the number of elements
-- At the type level, so we are sure we handle exactly the right amount.
type Vec2 a
  = Vec D2 a
