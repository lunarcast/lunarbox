module Lunarbox.Data.Editor.Constants
  ( nodeRadius
  , arcSpacing
  , arcWidth
  ) where

import Math (Radians)

-- visual radius for nodes
nodeRadius :: Number
nodeRadius = 50.0

-- How much spage to display between node inputs
arcSpacing :: Radians
arcSpacing = 0.1

-- What width should the stroke of node inputs have
arcWidth :: Number
arcWidth = 5.0
