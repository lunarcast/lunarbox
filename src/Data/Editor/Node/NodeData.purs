module Lunarbox.Data.Editor.Node.NodeData
  ( NodeData(..)
  , _NodeDataPosition
  , _NodeDataSelected
  , _NodeDataZPosition
  , _NodeDataComment
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Lunarbox.Data.Lens (newtypeIso)
import Lunarbox.Data.Vector (Vec2)

newtype NodeData
  = NodeData { position :: Vec2 Number, selected :: Boolean, zPosition :: Int, comment :: Maybe String }

derive instance newtypeNodeData :: Newtype NodeData _

derive instance eqNodeData :: Eq NodeData

derive instance genericNodeData :: Generic NodeData _

derive newtype instance encodeJsonNodeData :: EncodeJson NodeData

derive newtype instance decodeJsonNodeData :: DecodeJson NodeData

instance showNodeData :: Show NodeData where
  show = genericShow

instance ordNodeData :: Ord NodeData where
  compare (NodeData { zPosition }) (NodeData ({ zPosition: zPosition' })) = compare zPosition zPosition'

instance defaultNodeData :: Default NodeData where
  def = NodeData { position: zero, selected: false, zPosition: 0, comment: Nothing }

-- Lenses
_NodeDataPosition :: Lens' NodeData (Vec2 Number)
_NodeDataPosition = newtypeIso <<< prop (SProxy :: SProxy "position")

_NodeDataSelected :: Lens' NodeData Boolean
_NodeDataSelected = newtypeIso <<< prop (SProxy :: SProxy "selected")

_NodeDataZPosition :: Lens' NodeData Int
_NodeDataZPosition = newtypeIso <<< prop (SProxy :: _ "zPosition")

_NodeDataComment :: Lens' NodeData (Maybe String)
_NodeDataComment = newtypeIso <<< prop (SProxy :: _ "comment")
