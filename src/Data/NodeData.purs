module Lunarbox.Data.NodeData where

import Prelude
import Data.Lens (Lens', iso)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Vec (vec2)
import Lunarbox.Data.Vector (Vec2)

type MathVec2
  = Vec2 Number

type NodeDataContent
  = { position :: MathVec2, selected :: Boolean, scale :: MathVec2, image :: String }

newtype NodeData
  = NodeData NodeDataContent

derive instance newtypeNodeData :: Newtype NodeData _

instance semigroupNodeData :: Semigroup NodeData where
  append (NodeData { position, selected, scale, image }) (NodeData { position: position', selected: selected', scale: scale' }) =
    NodeData
      { position: position + position'
      , scale: scale + scale'
      , selected: selected && selected'
      , image
      }

instance monoidNodeData :: Monoid NodeData where
  mempty = NodeData { position: zero, scale: vec2 100.0 100.0, selected: false, image: "https://static.zerochan.net/Okabe.Rintarou.full.762874.jpg" }

-- Lenses
_NodeData :: Lens' NodeData NodeDataContent
_NodeData = iso unwrap wrap

_position :: Lens' NodeDataContent MathVec2
_position = prop (SProxy :: SProxy "position")

_selected :: Lens' NodeDataContent Boolean
_selected = prop (SProxy :: SProxy "selected")

_scale :: Lens' NodeDataContent MathVec2
_scale = prop (SProxy :: _ "scale")

_image :: Lens' NodeDataContent String
_image = prop (SProxy :: _ "image")

_NodeDataPosition :: Lens' NodeData MathVec2
_NodeDataPosition = _NodeData <<< _position

_NodeDataSelected :: Lens' NodeData Boolean
_NodeDataSelected = _NodeData <<< _selected

_NodeDataScale :: Lens' NodeData MathVec2
_NodeDataScale = _NodeData <<< _scale

_NodeDataImage :: Lens' NodeData String
_NodeDataImage = _NodeData <<< _image
