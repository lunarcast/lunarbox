module Lunarbox.Component.Editor.Scene
  ( Query(..)
  , Output(..)
  , component
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen (Component, HalogenM, RefLabel(..), defaultEval, gets, mkComponent, mkEval, modify_, raise, subscribe)
import Halogen.HTML as HH
import Halogen.HTML.Events (onDoubleClick, onMouseDown, onMouseMove, onMouseUp)
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Lunarbox.Component.Utils (className)
import Lunarbox.Config (Config)
import Lunarbox.Foreign.Render (Context2d, ForeignAction(..), GeomEventHandler, GeometryCache, emptyGeometryCache, handleDoubleClick, handleMouseDown, handleMouseMove, handleMouseUp, renderScene, resizeContext, withContext)
import Web.Event.Event (EventType(..))
import Web.HTML as Web
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
  | HandleResize a
  | Rerender a

data Output
  = BubbleForeignAction ForeignAction

canvasRef :: RefLabel
canvasRef = RefLabel "canvas"

component :: forall m. MonadEffect m => MonadAsk Config m => MonadAff m => Component HH.HTML Query Input Output m
component =
  mkComponent
    { initialState:
      const
        { context: Nothing
        -- NOTE: I made sure did is a safe operation to perform
        , geometryCache: unsafePerformEffect emptyGeometryCache
        }
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
  withContext' = withContext canvasRef

  handleAction :: Action -> HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    Init -> do
      withContext' $ const $ pure unit
      window <- liftEffect Web.window
      -- Registr resize events
      void $ subscribe
        $ ES.eventListenerEventSource
            (EventType "resize")
            (Window.toEventTarget window)
            (const $ Just ResizeCanvas)
    Render ->
      withContext' \ctx -> do
        cache <- gets _.geometryCache
        liftEffect $ renderScene ctx cache
    ResizeCanvas -> do
      withContext' $ liftEffect <<< resizeContext
      handleAction Render
    HandleEvent handler event ->
      withContext' \ctx -> do
        cache <- gets _.geometryCache
        action <- liftEffect $ handler ctx event cache
        handleAction $ HandleForeignAction action
    HandleForeignAction action -> case action of
      NoAction -> pure unit
      _ -> raise $ BubbleForeignAction action

  handleQuery :: forall a. Query a -> HalogenM State Action ChildSlots Output m (Maybe a)
  handleQuery = case _ of
    LoadScene cache a -> do
      modify_ _ { geometryCache = cache }
      handleAction Render
      pure $ Just a
    Rerender a -> do
      handleAction Render
      pure $ Just a
    HandleResize a -> do
      handleAction ResizeCanvas
      pure $ Just a

  -- This only renders the canvas, the rest of the rendering is done via some typescript code
  render =
    const
      $ HH.canvas
          [ HP.ref canvasRef
          , className "scene__canvas"
          , onMouseMove $ Just <<< HandleEvent handleMouseMove
          , onMouseUp $ Just <<< HandleEvent handleMouseUp
          , onMouseDown $ Just <<< HandleEvent handleMouseDown
          , onDoubleClick $ Just <<< HandleEvent handleDoubleClick
          ]
