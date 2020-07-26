module Lunarbox.Data.Editor.Class.Depends where

import Data.Set as Set

-- Typeclass representing something which depens on other dependencies
class Depends f a where
  getDependencies :: f -> Set.Set a
