module Lunarbox.Data.Editor.Node.NodeData
  ( CompatNodeData
  , NodeData(..)
  , defaultComment
  , _NodeDataPosition
  , _NodeDataSelected
  , _NodeDataZPosition
  , _NodeDataComment
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, (.!=), (.:?))
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Lunarbox.Data.Editor.Node.CommentData (CommentData)
import Lunarbox.Data.Lens (newtypeIso)
import Lunarbox.Data.Vector (Vec2)
import Record as Record

-- Node data from the first lunarbox version. Used to keep backwards compatibility
type CompatNodeData r
  = ( position :: Vec2 Number, selected :: Boolean, zPosition :: Int
    | r
    )

-- Newer versions of lunarbox also have the comment field
newtype NodeData
  = NodeData { | CompatNodeData ( comment :: Maybe CommentData ) }

derive instance newtypeNodeData :: Newtype NodeData _

derive instance eqNodeData :: Eq NodeData

derive instance genericNodeData :: Generic NodeData _

derive newtype instance encodeJsonNodeData :: EncodeJson NodeData

instance decodeJsonNodeData :: DecodeJson NodeData where
  decodeJson json = do
    -- Decode the part all versions of lunarbox have
    others :: { | CompatNodeData () } <- decodeJson json
    -- Decode comments which were added in 1.18
    obj <- decodeJson json
    comment <- obj .:? "comment" .!= Nothing
    -- Merge everything together
    pure $ NodeData $ Record.merge { comment } others

instance showNodeData :: Show NodeData where
  show = genericShow

instance ordNodeData :: Ord NodeData where
  compare (NodeData { zPosition }) (NodeData ({ zPosition: zPosition' })) = compare zPosition zPosition'

instance defaultNodeData :: Default NodeData where
  def = NodeData { position: zero, selected: false, zPosition: 0, comment: Nothing }

-- Same as def but also keeps a comment
defaultComment :: NodeData
defaultComment = set _NodeDataComment (Just def) def

-- Lenses
_NodeDataPosition :: Lens' NodeData (Vec2 Number)
_NodeDataPosition = newtypeIso <<< prop (SProxy :: SProxy "position")

_NodeDataSelected :: Lens' NodeData Boolean
_NodeDataSelected = newtypeIso <<< prop (SProxy :: SProxy "selected")

_NodeDataZPosition :: Lens' NodeData Int
_NodeDataZPosition = newtypeIso <<< prop (SProxy :: _ "zPosition")

_NodeDataComment :: Lens' NodeData (Maybe CommentData)
_NodeDataComment = newtypeIso <<< prop (SProxy :: _ "comment")
