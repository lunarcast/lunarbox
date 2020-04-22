module Lunarbox.Data.Editor.Node.NodeId where

import Prelude
import Data.Default (class Default)
import Data.Lens (Lens')
import Data.Newtype (class Newtype, unwrap)
import Lunarbox.Data.Dataflow.Class.Expressible (class Expressible, toExpression)
import Lunarbox.Data.Lens (newtypeIso)

newtype NodeId
  = NodeId String

derive instance eqNodeId :: Eq NodeId

derive instance ordNodeId :: Ord NodeId

derive instance newtypeNodeId :: Newtype NodeId _

instance defaultNodeId :: Default NodeId where
  def = NodeId ""

instance showNodeId :: Show NodeId where
  show = unwrap

instance expressibleNodeId :: Expressible NodeId Unit where
  toExpression = toExpression <<< unwrap

_NodeId :: Lens' NodeId String
_NodeId = newtypeIso
