module Lunarbox.Data.Editor.Node.PinLocation
  ( Pin(..)
  , ScopedLocation(..)
  , inputNode
  , outputNode
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Lunarbox.Data.Dataflow.Expression (Expression(..))
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.String (showIndex)

-- A pin can either be an output or an input
data Pin
  = InputPin Int
  | OutputPin

derive instance eqPin :: Eq Pin

derive instance ordPin :: Ord Pin

derive instance genericPin :: Generic Pin _

instance encodeJsonPin :: EncodeJson Pin where
  encodeJson = genericEncodeJson

instance decodeJsonPin :: DecodeJson Pin where
  decodeJson = genericDecodeJson

instance showPin :: Show Pin where
  show OutputPin = "output"
  show (InputPin index) = "input-" <> show index

-- This is either the location of a node or the location of a pin
data ScopedLocation
  = NodeLocation NodeId
  | UnexistingNode NodeId
  | NodeDefinition NodeId
  | FunctionDeclaration
  | PinLocation NodeId Pin
  | FunctionUsage NodeId FunctionName
  | AtApplication NodeId Int
  | InsideNative
  | PlaceholderPosition

derive instance eqScopedLocation :: Eq ScopedLocation

derive instance ordScopedLocation :: Ord ScopedLocation

derive instance genericScopedLocation :: Generic ScopedLocation _

instance encodeJsonScopedLocation :: EncodeJson ScopedLocation where
  encodeJson = genericEncodeJson

instance decodeJsonScopedLocation :: DecodeJson ScopedLocation where
  decodeJson = genericDecodeJson

instance showScopedLocation :: Show ScopedLocation where
  show PlaceholderPosition = "(This position is a placeholder. If you see this then please open an issue on github)"
  show InsideNative = "inside a native function"
  show FunctionDeclaration = "at the declaration of a function"
  show (AtApplication id index) = "at the " <> showIndex index <> " input of node " <> show id
  show (NodeDefinition id) = "at definition of node " <> show id
  show (FunctionUsage id name) = "at function reference " <> show name <> " in node " <> show name
  show (NodeLocation id) = "at node " <> show id
  show (UnexistingNode id) = "at node " <> show id <> " which doesn't exist"
  show (PinLocation id OutputPin) = "at the output of node " <> show id
  show (PinLocation id (InputPin index)) = "at the " <> showIndex index <> " input of node " <> show id

-- This is an internal function used to both mark a node and one of it's pins
mark :: Pin -> NodeId -> Expression ScopedLocation -> Expression ScopedLocation
mark pin id = Expression location <<< Expression pinLocation
  where
  location = NodeLocation id

  pinLocation = PinLocation id pin

-- Wrap an input node and mark the position of it's output
inputNode :: NodeId -> Expression ScopedLocation -> Expression ScopedLocation
inputNode = mark OutputPin

-- Wrap an output node and mark the positions of it's input
outputNode :: NodeId -> Expression ScopedLocation -> Expression ScopedLocation
outputNode = mark $ InputPin 0
