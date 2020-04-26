module Lunarbox.Data.Editor.Node.PinLocation
  ( Pin(..)
  , NodeOrPinLocation
  , inputNode
  , outputNode
  ) where

import Lunarbox.Data.Dataflow.Expression (Expression, wrap)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..))
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Prelude

-- A pin can either be an output or an input
data Pin
  = InputPin Int
  | OutputPin

derive instance eqPin :: Eq Pin

derive instance ordPin :: Ord Pin

instance showPin :: Show Pin where
  show OutputPin = "output"
  show (InputPin index) = "input-" <> show index

-- This is either the location of a node or the location of a pin
type NodeOrPinLocation
  = ExtendedLocation NodeId Pin

-- This is an internal function used to both mark a node and one of it's pins
mark :: Pin -> NodeId -> Expression NodeOrPinLocation -> Expression NodeOrPinLocation
mark pin id =
  let
    location = Location id

    pinLocation = DeepLocation id pin
  in
    wrap location <<< wrap pinLocation

-- Wrap an input node and mark the position of it's output
inputNode :: NodeId -> Expression NodeOrPinLocation -> Expression NodeOrPinLocation
inputNode = mark OutputPin

-- Wrap an output node and mark the positions of it's input
outputNode :: NodeId -> Expression NodeOrPinLocation -> Expression NodeOrPinLocation
outputNode = mark $ InputPin 0