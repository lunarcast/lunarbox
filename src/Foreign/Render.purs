module Lunarbox.Foreign.Render where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, Json)
import Data.Default (class Default)
import Data.Either (Either(..))
import Data.Nullable (Nullable)
import Effect (Effect)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Web.HTML (HTMLCanvasElement)
import Web.UIEvent.MouseEvent (MouseEvent)

-- Foreign data types
-- This is the rendering context used to render on a canvas
foreign import data Context2d :: Type

-- This is just a map of cached node geometries
foreign import data GeomteryCache :: Type

-- Foreign helpers
foreign import resizeCanvas :: HTMLCanvasElement -> Effect Unit

foreign import resizeContext :: Context2d -> Effect Unit

foreign import getContext :: HTMLCanvasElement -> Effect Context2d

foreign import renderScene :: Context2d -> GeomteryCache -> Effect Unit

foreign import emptyGeometryCache :: GeomteryCache

foreign import handleMouseMove :: GeomEventHandler

foreign import handleMouseUp :: GeomEventHandler

foreign import handleMouseDown :: GeomEventHandler

foreign import geometryCacheFromJsonImpl :: ForeignEitherConfig String GeomteryCache -> Json -> Either String GeomteryCache

foreign import geometryCacheToJson :: GeomteryCache -> Json

foreign import createNode :: GeomteryCache -> NodeId -> Int -> Effect Unit

foreign import refreshInputArcs :: GeomteryCache -> NodeId -> Array InputData -> Effect Unit

instance defaultGeomtryCache :: Default GeomteryCache where
  -- WARNING: this might create spooky actions at a distance!!! 
  -- (This doesn't happen anywhere curently but I should keep it in mind)
  def = emptyGeometryCache

instance decodeJsonGeometryCache :: DecodeJson GeomteryCache where
  -- The error messages in for this are just the Error.message from js land
  -- so idk how clear those are, maybe I should somehow make it check if the structure is right
  -- and _then_ try parsing it, but that's something to do for the next time
  decodeJson = geometryCacheFromJsonImpl { left: Left, right: Right }

instance encodeJsonGeometryCache :: EncodeJson GeomteryCache where
  encodeJson = geometryCacheToJson

-- | This is the data the typescript part needs to render the input arcs
type InputData
  = { color :: String
    , output :: Nullable NodeId
    }

-- | Some foreign stuff might error out 
-- | so we pass this to it to inform it of how we handle errors 
type ForeignEitherConfig e a
  = { left :: e -> Either e a
    , right :: a -> Either e a
    }

type GeomEventHandler
  = Context2d -> MouseEvent -> GeomteryCache -> Effect Unit
