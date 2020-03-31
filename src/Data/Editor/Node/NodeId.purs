module Lunarbox.Data.Editor.Node.NodeId where

import Prelude
import Data.Lens (Lens')
import Data.Newtype (class Newtype, unwrap)
import Lunarbox.Data.Dataflow.Class.Expressible (class Expressible, toExpression)
import Lunarbox.Data.Lens (newtypeIso)

newtype NodeId
  = NodeId String

derive instance eqNodeId :: Eq NodeId

derive instance ordNodeId :: Ord NodeId

derive instance newtypeNodeId :: Newtype NodeId _

derive newtype instance showNodeId :: Show NodeId

instance expressibleNodeId :: Expressible NodeId Unit where
  toExpression = toExpression <<< unwrap

_NodeId :: Lens' NodeId String
_NodeId = newtypeIso
