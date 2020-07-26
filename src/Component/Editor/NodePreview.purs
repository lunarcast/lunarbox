module Lunarbox.Component.Editor.NodePreview
  ( Query(..)
  , Output(..)
  , component
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (Component, HalogenM, RefLabel(..), defaultEval, gets, mkComponent, mkEval, raise)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (className)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Foreign.Render (Context2d, ForeignTypeMap, renderPreview, withContext)
import Record as Record

data Action
  = Init

type ChildSlots
  = ()

type Input r
  = ( name :: FunctionName | r )

type State
  = { | Input ( context :: Maybe Context2d ) }

data Query a
  = Rerender ForeignTypeMap a

data Output
  = RequestRerender

canvasRef :: FunctionName -> RefLabel
canvasRef id = RefLabel $ "canvas-" <> show id

component :: forall m. MonadEffect m => Component HH.HTML Query { | Input () } Output m
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
  handleAction :: Action -> HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    Init ->
      canvasRef <$> gets _.name
        >>= \ref ->
            withContext ref \_ -> raise RequestRerender

  handleQuery :: forall a. Query a -> HalogenM State Action ChildSlots Output m (Maybe a)
  handleQuery = case _ of
    Rerender colorMap result -> do
      ref <- canvasRef <$> gets _.name
      withContext ref \ctx -> liftEffect $ renderPreview ctx colorMap
      pure $ Just result

  -- This only renders the canvas, the rest of the rendering is done via some typescript code
  render { name } =
    HH.canvas
      [ HP.width 75
      , HP.height 75
      , HP.ref $ canvasRef name
      , className "node__preview"
      ]
