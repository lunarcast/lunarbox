module Lunarbox.Data.Dataflow.NodeId where

import Prelude
import Data.Lens (Lens')
import Data.Newtype (class Newtype)
import Lunarbox.Data.Lens (newtypeIso)
import Lunarbox.Dataflow.Expressible (class Expressible)

newtype NodeId
  = NodeId String

derive instance eqNodeId :: Eq NodeId

derive instance ordNodeId :: Ord NodeId

derive instance newtypeNodeId :: Newtype NodeId _

derive newtype instance showNodeId :: Show NodeId

derive newtype instance expressibleNodeId :: Expressible NodeId

_NodeId :: Lens' NodeId String
_NodeId = newtypeIso
