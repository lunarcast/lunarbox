module Lunarbox.Foreign.Render
  ( Context2d
  , GeometryCache
  , GeomEventHandler
  , NodeState
  , ForeignTypeMap
  , InputData
  , ForeignAction(..)
  , resizeCanvas
  , resizeContext
  , getContext
  , renderScene
  , handleMouseMove
  , handleMouseDown
  , handleMouseUp
  , createNode
  , refreshInputArcs
  , emptyGeometryCache
  , setUnconnectableInputs
  , setUnconnectableOutputs
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, Json)
import Data.Default (class Default, def)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn3, Fn4, mkFn2, mkFn3, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Nullable (Nullable)
import Effect (Effect)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Set (NativeSet)
import Web.HTML (HTMLCanvasElement)
import Web.UIEvent.MouseEvent (MouseEvent)

-- Foreign data types
-- This is the rendering context used to render on a canvas
foreign import data Context2d :: Type

-- This is just a map of cached node geometries
foreign import data GeometryCache :: Type

-- Foreign helpers
foreign import resizeCanvas :: HTMLCanvasElement -> Effect Unit

foreign import resizeContext :: Context2d -> Effect Unit

foreign import getContext :: HTMLCanvasElement -> Effect Context2d

foreign import renderScene :: Context2d -> GeometryCache -> Effect Unit

foreign import emptyGeometryCache :: Effect GeometryCache

foreign import handleMouseMoveImpl :: NativeGeomEventHandler

foreign import handleMouseUpImpl :: NativeGeomEventHandler

foreign import handleMouseDownImpl :: NativeGeomEventHandler

foreign import geometryCacheFromJsonImpl :: ForeignEitherConfig String GeometryCache -> Json -> Either String GeometryCache

foreign import geometryCacheToJson :: GeometryCache -> Json

foreign import createNode :: GeometryCache -> NodeId -> Int -> Boolean -> Effect Unit

foreign import refreshInputArcs :: GeometryCache -> NodeId -> NodeState -> Effect Unit

foreign import setUnconnectableInputs :: GeometryCache -> NativeSet { id :: NodeId, index :: Int } -> Effect Unit

foreign import setUnconnectableOutputs :: GeometryCache -> NativeSet NodeId -> Effect Unit

instance decodeJsonGeometryCache :: DecodeJson GeometryCache where
  -- WARNING:
  -- The error messages in for this are just the Error.message from js land
  -- so idk how clear those are, maybe I should somehow make it check if the structure is right
  -- and _then_ try parsing it, but that's something to do for the next time
  decodeJson = geometryCacheFromJsonImpl { left: Left, right: Right }

instance encodeJsonGeometryCache :: EncodeJson GeometryCache where
  encodeJson = geometryCacheToJson

-- | Type of event handlers for the Scene component.
type NativeGeomEventHandler
  = Fn4 ForeignActionConfig Context2d MouseEvent GeometryCache (Effect ForeignAction)

-- | Curried version of NativeGeomEventHandler.
type GeomEventHandler
  = Context2d -> MouseEvent -> GeometryCache -> Effect ForeignAction

-- | Some foreign stuff might error out 
-- | so we pass this to ts to inform it how we handle errors 
type ForeignEitherConfig e a
  = { left :: e -> Either e a
    , right :: a -> Either e a
    }

-- | This is the data the typescript part needs to render the input arcs
type InputData
  = Nullable NodeId

-- | Data related to colors we need for updating some geometries
type ForeignTypeMap
  = { inputs :: Array (Nullable String)
    , output :: Nullable String
    }

-- | Data needed for updating the geometry of a ndoe
type NodeState
  = { inputs :: Array InputData
    , colorMap :: ForeignTypeMap
    , value :: Nullable String
    }

-- | Stuff the ts side of things can tell us to do
data ForeignAction
  -- Create a connection from output-id to input-id, input-index
  = CreateConnection NodeId NodeId Int
  | SelectInput NodeId Int
  | SelectOutput NodeId
  | DeleteConnection NodeId Int
  -- This just does nothing
  | NoAction

derive instance genericForeignAction :: Generic ForeignAction _

instance showForeignAction :: Show ForeignAction where
  show = genericShow

-- | Config used by the ts side to create actions
newtype ForeignActionConfig
  = ForeignActionConfig
  { createConnection :: Fn3 NodeId NodeId Int ForeignAction
  , selectInput :: Fn2 NodeId Int ForeignAction
  , deleteConnection :: Fn2 NodeId Int ForeignAction
  , selectOutput :: NodeId -> ForeignAction
  , nothing :: ForeignAction
  }

instance defaultForeignActionConfig :: Default ForeignActionConfig where
  def =
    ForeignActionConfig
      { createConnection: mkFn3 CreateConnection
      , selectInput: mkFn2 SelectInput
      , deleteConnection: mkFn2 DeleteConnection
      , selectOutput: SelectOutput
      , nothing: NoAction
      }

-- | Handle a mouse down event
handleMouseDown :: GeomEventHandler
handleMouseDown = runFn4 handleMouseDownImpl $ def

-- | Handle a mouse up event
handleMouseUp :: GeomEventHandler
handleMouseUp = runFn4 handleMouseUpImpl $ def

-- | Handle a mouse move event
handleMouseMove :: GeomEventHandler
handleMouseMove = runFn4 handleMouseMoveImpl $ def
