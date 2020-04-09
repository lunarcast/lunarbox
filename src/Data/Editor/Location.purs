module Lunarbox.Data.Editor.Location (Location) where

import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node.PinLocation (NodeOrPinLocation)

-- Location for stuff in Projects
type Location
  = ExtendedLocation FunctionName NodeOrPinLocation
