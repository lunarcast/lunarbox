module Lunarbox.Foreign.Render where

import Prelude
import Control.Monad.State (gets, modify_)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json)
import Data.Default (class Default, def)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn3, Fn4, mkFn2, mkFn3, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM, RefLabel, getHTMLElementRef, liftEffect)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Set (NativeSet)
import Web.HTML (HTMLCanvasElement)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
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

foreign import createNode :: GeometryCache -> NodeId -> Int -> Boolean -> Nullable String -> Effect Unit

foreign import refreshInputArcs :: GeometryCache -> NodeId -> NodeState -> Effect Unit

foreign import setUnconnectableInputs :: GeometryCache -> NativeSet { id :: NodeId, index :: Int } -> Effect Unit

foreign import setUnconnectableOutputs :: GeometryCache -> NativeSet NodeId -> Effect Unit

foreign import deleteNode :: GeometryCache -> NodeId -> Effect Unit

foreign import renderPreview :: Context2d -> ForeignTypeMap -> Effect Unit

foreign import centerNode :: GeometryCache -> NodeId -> Effect Unit

foreign import centerOutput :: GeometryCache -> Effect Unit

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

-- Halogen M which keeps track of a canvas
type CanvasHalogenM r a s o m a'
  = HalogenM { | ( context :: Maybe Context2d | r ) } a s o m a'

-- | Run a computation (inside a halogen component) which requires access to a canvas rendering context.
withContext ::
  forall r a s o m.
  MonadEffect m =>
  RefLabel -> (Context2d -> CanvasHalogenM r a s o m Unit) -> CanvasHalogenM r a s o m Unit
withContext ref continue = do
  context <- gets _.context
  case context of
    Just ctx -> continue ctx
    Nothing -> do
      element <- (_ >>= HTMLCanvasElement.fromHTMLElement) <$> getHTMLElementRef ref
      case element of
        Nothing -> pure unit
        Just canvas -> do
          liftEffect $ resizeCanvas canvas
          ctx <- liftEffect $ getContext canvas
          modify_ _ { context = Just ctx }
          continue ctx
