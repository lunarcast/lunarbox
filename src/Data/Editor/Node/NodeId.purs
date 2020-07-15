module Lunarbox.Data.Editor.Node.NodeId where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Default (class Default)
import Data.Lens (Lens')
import Data.Newtype (class Newtype, unwrap)
import Lunarbox.Data.Lens (newtypeIso)

newtype NodeId
  = NodeId String

derive instance eqNodeId :: Eq NodeId

derive instance ordNodeId :: Ord NodeId

derive instance newtypeNodeId :: Newtype NodeId _

derive newtype instance encodeJsonNodeId :: EncodeJson NodeId

derive newtype instance decodeJsonNodeId :: DecodeJson NodeId

derive newtype instance semigroupNodeId :: Semigroup NodeId

instance defaultNodeId :: Default NodeId where
  def = NodeId ""

instance showNodeId :: Show NodeId where
  show = unwrap

_NodeId :: Lens' NodeId String
_NodeId = newtypeIso
