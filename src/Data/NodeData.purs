module Lunarbox.Data.NodeData where

import Prelude
import Data.Lens (Lens', iso)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Typelevel.Num (D2)
import Data.Vec (Vec)

type Vec2 a
  = Vec D2 a

type MathVec2
  = Vec2 Number

type NodeDataContent
  = { position :: MathVec2, selected :: Boolean }

newtype NodeData
  = NodeData NodeDataContent

derive instance newtypeNodeData :: Newtype NodeData _

instance semigroupNodeData :: Semigroup NodeData where
  append (NodeData { position, selected }) (NodeData { position: position', selected: selected' }) =
    NodeData
      { position: position + position', selected: selected && selected' }

instance monoidNodeData :: Monoid NodeData where
  mempty = NodeData { position: zero, selected: false }

-- Lenses
_NodeData :: Lens' NodeData NodeDataContent
_NodeData = iso unwrap wrap

_position :: Lens' NodeDataContent MathVec2
_position = prop (SProxy :: SProxy "position")

_selected :: Lens' NodeDataContent Boolean
_selected = prop (SProxy :: SProxy "selected")

_NodeDataPosition :: Lens' NodeData MathVec2
_NodeDataPosition = _NodeData <<< _position

_NodeDataSelected :: Lens' NodeData Boolean
_NodeDataSelected = _NodeData <<< _selected
