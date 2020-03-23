module Lunarbox.Component.Editor.Node where

import Prelude
import Data.Lens (Lens', over, set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, defaultEval, gets, mkComponent, mkEval, modify_, raise)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown)
import Lunarbox.Component.Editor (Query(..))
import Lunarbox.Data.NodeData (NodeData(..), MathVec2, _NodeDataSelected, _NodeDataPosition)
import Lunarbox.Data.Project (Node)
import Lunarbox.Data.Vector (Vec2)
import Svg.Attributes as SA
import Svg.Elements as SE

type State
  = { nodeData :: NodeData
    , node :: Node
    }

_nodeData :: Lens' State NodeData
_nodeData = prop (SProxy :: SProxy "nodeData")

_position :: Lens' State MathVec2
_position = _nodeData <<< _NodeDataPosition

_stateSelected :: Lens' State Boolean
_stateSelected = _nodeData <<< _NodeDataSelected

data Action
  = SetSelection Boolean

data Query a
  = Unselect a
  | GetData a
  | Drag (Vec2 Number)

type ChildSlots
  = ()

type Output
  = Void

type Input
  = Tuple Node NodeData

component :: forall m. MonadEffect m => Component HH.HTML Query Input Output m
component =
  mkComponent
    { initialState: \(Tuple node nodeData) -> { node, nodeData }
    , render
    , eval:
        mkEval
          $ defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              }
    }
  where
  handleAction :: Action -> HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    SetSelection value -> do
      modify_ $ set _stateSelected value

  handleQuery :: forall a. Query a -> HalogenM State Action ChildSlots Output m (Maybe a)
  handleQuery = case _ of
    Unselect inner -> do
      modify_ $ set _stateSelected false
      pure $ Just inner
    Drag offest -> do
      modify_ $ over _position ((+) offest)
      pure Nothing
    -- This runs when the Scene component wants us to save the data
    Save inner -> do
      nodeData <- gets $ view _nodeData
      pure $ Just nodeData

  render ({ nodeData: NodeData { position, selected } }) =
    SE.rect
      [ SA.width 100.0
      , SA.height 100.0
      -- TODO: remove this, for debugging only
      , SA.fill $ Just $ if selected then SA.RGB 128 255 255 else SA.RGB 255 255 255
      , SA.x $ position !! d0
      , SA.y $ position !! d1
      , SA.stroke $ Just $ if selected then SA.RGB 118 255 2 else SA.RGB 63 196 255
      , onMouseDown $ const $ Just $ SetSelection true
      ]
