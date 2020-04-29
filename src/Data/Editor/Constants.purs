module Lunarbox.Data.Editor.Constants
  ( nodeRadius
  , arcSpacing
  , arcWidth
  , inputLayerOffset
  , connectionsWidth
  , scaleConnectionPreview
  , mouseId
  , scrollStep
  , clampZoom
  ) where

import Prelude
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
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

-- How much space to keep between 2 input layers
inputLayerOffset :: Number
inputLayerOffset = 10.0

-- The stroke width of the wires
connectionsWidth :: Number
connectionsWidth = 5.0

-- THis is required so the preview doesn't catch all the events
scaleConnectionPreview :: Number -> Number
scaleConnectionPreview = (_ / 1.01)

-- Used for connection previews
mouseId :: NodeId
mouseId = NodeId "mouse"

-- How much to scroll per event
scrollStep :: Number
scrollStep = 1.0 / 1.2

-- Clamp the zoom between those values
clampZoom :: Number -> Number
clampZoom = clamp 0.1 10.0
