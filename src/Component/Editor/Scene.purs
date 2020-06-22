module Lunarbox.Component.Editor.Scene (component, Query(..)) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Default (def)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (Component, HalogenM, RefLabel(..), defaultEval, getHTMLElementRef, gets, mkComponent, mkEval, modify_, subscribe)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown, onMouseMove, onMouseUp)
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Lunarbox.Config (Config)
import Lunarbox.Control.Monad.Effect (print)
import Lunarbox.Foreign.Render (Context2d, ForeignAction(..), GeomEventHandler, GeometryCache, getContext, handleMouseDown, handleMouseMove, handleMouseUp, renderScene, resizeCanvas, resizeContext)
import Web.Event.Event (EventType(..))
import Web.HTML as Web
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)

type State
  = { context :: Maybe Context2d, geometryCache :: GeometryCache }

data Action
  = Init
  | Render
  | ResizeCanvas
  | HandleEvent GeomEventHandler MouseEvent
  | HandleForeignAction ForeignAction

type ChildSlots
  = ()

type Input
  = Unit

data Query a
  = LoadScene GeometryCache a
  | Rerender a

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
    HandleEvent handler event ->
      withContext \ctx -> do
        cache <- gets _.geometryCache
        action <- liftEffect $ handler ctx event cache
        handleAction $ HandleForeignAction action
    HandleForeignAction action -> case action of
      CreateConnection from toId toIndex -> do
        print
          { from, toId, toIndex
          }
      _ -> pure unit

  handleQuery :: forall a. Query a -> HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    LoadScene cache a -> do
      modify_ _ { geometryCache = cache }
      handleAction Render
      pure $ Just a
    Rerender a -> do
      handleAction Render
      pure $ Just a

  -- This only renders the canvas, the rest of the rendering is done via some typescript code
  render =
    const
      $ HH.canvas
          [ HP.ref canvasRef
          , onMouseMove $ Just <<< HandleEvent handleMouseMove
          , onMouseUp $ Just <<< HandleEvent handleMouseUp
          , onMouseDown $ Just <<< HandleEvent handleMouseDown
          ]
