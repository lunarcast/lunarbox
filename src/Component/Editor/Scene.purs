module Lunarbox.Component.Editor.Scene (component, Query(..)) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Default (def)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (Component, HalogenM, RefLabel(..), defaultEval, getHTMLElementRef, gets, mkComponent, mkEval, modify_, subscribe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Lunarbox.Config (Config)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Foreign.Render (Context2d, GeomteryCache, NodeRenderingData, getContext, loadNodes, renderScene, resizeCanvas, resizeContext)
import Web.Event.Event (EventType(..))
import Web.HTML as Web
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.Window as Window

type State
  = { context :: Maybe Context2d, geometryCache :: GeomteryCache }

data Action
  = Init
  | Render
  | ResizeCanvas

type ChildSlots
  = ()

type Input
  = Unit

data Query a
  = LoadNodes (List (Tuple NodeId NodeRenderingData)) a

canvasRef :: RefLabel
canvasRef = RefLabel "canvas"

component :: forall m o. MonadEffect m => MonadAsk Config m => MonadAff m => Component HH.HTML Query Input o m
component =
  mkComponent
    { initialState: const { context: Nothing, geometryCache: def }
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            , initialize = Just Init
            }
    }
  where
  withContext :: (Context2d -> HalogenM State Action ChildSlots o m Unit) -> HalogenM State Action ChildSlots o m Unit
  withContext continue = do
    context <- gets _.context
    case context of
      Just ctx -> continue ctx
      Nothing -> do
        element <- (_ >>= HTMLCanvasElement.fromHTMLElement) <$> getHTMLElementRef canvasRef
        case element of
          Nothing -> pure unit
          Just canvas -> do
            liftEffect $ resizeCanvas canvas
            ctx <- liftEffect $ getContext canvas
            modify_ _ { context = Just ctx }
            continue ctx

  handleAction :: Action -> HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Init -> do
      withContext $ const $ pure unit
      window <- liftEffect Web.window
      -- Registr resize events
      void $ subscribe
        $ ES.eventListenerEventSource
            (EventType "resize")
            (Window.toEventTarget window)
            (const $ Just ResizeCanvas)
    Render ->
      withContext \ctx -> do
        cache <- gets _.geometryCache
        liftEffect $ renderScene ctx cache
    ResizeCanvas -> do
      withContext $ liftEffect <<< resizeContext
      handleAction Render

  handleQuery :: forall a. Query a -> HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    LoadNodes nodes a -> do
      cache <- gets _.geometryCache
      liftEffect $ loadNodes cache nodes
      handleAction Render
      pure $ Just a

  -- This only renders the canvas, the rest of the rendering is done via some typescript code
  render = const $ HH.canvas [ HP.ref canvasRef ]
