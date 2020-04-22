module Lunarbox.Data.Editor.Node.NodeData
  ( NodeData(..)
  , NodeDataContent
  , _NodeDataPosition
  , _NodeDataSelected
  , _NodeDataZPosition
  ) where

import Prelude
import Data.Default (class Default)
import Data.Lens (Lens', iso)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (SProxy(..))
import Lunarbox.Data.Vector (Vec2)

-- This is here so we don't have to rewrite this type in the _NodeData lens
type NodeDataContent
  = { position :: Vec2 Number, selected :: Boolean, zPosition :: Int }

newtype NodeData
  = NodeData NodeDataContent

derive instance newtypeNodeData :: Newtype NodeData _

derive instance eqNodeData :: Eq NodeData

instance ordNodeData :: Ord NodeData where
  compare (NodeData { zPosition }) (NodeData ({ zPosition: zPosition' })) = compare zPosition zPosition'

instance defaultNodeData :: Default NodeData where
  def = NodeData { position: zero, selected: false, zPosition: 0 }

-- Lenses
_NodeData :: Lens' NodeData NodeDataContent
_NodeData = iso unwrap wrap

_NodeDataPosition :: Lens' NodeData (Vec2 Number)
_NodeDataPosition = _NodeData <<< prop (SProxy :: SProxy "position")

_NodeDataSelected :: Lens' NodeData Boolean
_NodeDataSelected = _NodeData <<< prop (SProxy :: SProxy "selected")

_NodeDataZPosition :: Lens' NodeData Int
_NodeDataZPosition = _NodeData <<< prop (SProxy :: _ "zPosition")
