module Lunarbox.Data.String (indent) where

import Prelude
import Data.String.Utils (lines, unsafeRepeat)
import Data.String (joinWith)

-- Indent a string by a number of spaces
indent :: Int -> String -> String
indent spaces = joinWith "\n" <<< map (space <> _) <<< lines
  where
  space = unsafeRepeat spaces " "
