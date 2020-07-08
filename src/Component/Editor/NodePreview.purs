module Lunarbox.Component.Editor.NodePreview
  ( Query(..)
  , component
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (Component, HalogenM, RefLabel(..), defaultEval, gets, mkComponent, mkEval)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (className)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Foreign.Render (Context2d, ForeignTypeMap, renderPreview, withContext)
import Record as Record

data Action
  = Init

type ChildSlots
  = ()

type Input r
  = ( id :: NodeId | r )

type State
  = { | Input ( context :: Maybe Context2d ) }

data Query a
  = Rerender ForeignTypeMap a

canvasRef :: NodeId -> RefLabel
canvasRef id = RefLabel $ "canvas-" <> show id

component :: forall m o. MonadEffect m => Component HH.HTML Query { | Input () } o m
component =
  mkComponent
    { initialState: Record.merge { context: Nothing }
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
  handleAction :: Action -> HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Init ->
      canvasRef <$> gets _.id
        >>= \ref ->
            withContext ref $ const $ pure unit

  handleQuery :: forall a. Query a -> HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    Rerender colorMap result -> do
      ref <- canvasRef <$> gets _.id
      withContext ref \ctx -> liftEffect $ renderPreview ctx colorMap
      pure $ Just result

  -- This only renders the canvas, the rest of the rendering is done via some typescript code
  render { id } =
    HH.canvas
      [ HP.width 75
      , HP.height 75
      , HP.ref $ canvasRef id
      , className "node__preview"
      ]
