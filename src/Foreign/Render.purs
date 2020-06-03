module Lunarbox.Foreign.Render where

import Prelude
import Control.Apply (applyFirst)
import Data.Default (class Default)
import Data.List (List, foldr)
import Data.Nullable (Nullable)
import Data.Tuple (Tuple, uncurry)
import Effect (Effect)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Vector (Vec2)
import Web.HTML (HTMLCanvasElement)

-- Foreign data types
-- This is the rendering context used to render on a canvas
foreign import data Context2d :: Type

-- This is just a map of cached node geometries
foreign import data GeomteryCache :: Type

-- Foreign helpers
foreign import resizeCanvas :: HTMLCanvasElement -> Effect Unit

foreign import getContext :: HTMLCanvasElement -> Effect Context2d

foreign import renderScene :: Context2d -> SceneData -> Effect Unit

foreign import loadNode :: GeomteryCache -> NodeId -> NodeRenderingData -> Effect Unit

foreign import emptyGeometryCache :: GeomteryCache

instance defaultGeomtryCache :: Default GeomteryCache where
  def = emptyGeometryCache

type InputData
  = { output :: Nullable String
    , color :: String
    , arc :: Vec2 Number
    , value :: Nullable NodeId
    }

type NodeRenderingData
  = { position :: Vec2 Number
    , inputs :: Array (Array InputData)
    }

type SceneData
  = { nodes :: Array NodeRenderingData
    }

-- Load more nodes in the same cache
loadNodes :: GeomteryCache -> List (Tuple NodeId NodeRenderingData) -> Effect Unit
loadNodes cache = foldr (applyFirst <<< go) $ pure unit
  where
  go = uncurry $ loadNode cache
