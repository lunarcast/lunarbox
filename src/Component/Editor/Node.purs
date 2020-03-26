module Lunarbox.Component.Editor.Node where

import Prelude
import Data.Lens (Lens', over, set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, defaultEval, gets, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown)
import Lunarbox.Data.NodeData (MathVec2, NodeData(..), _NodeDataPosition, _NodeDataSelected)
import Lunarbox.Data.Project (Node)
import Lunarbox.Data.Vector (Vec2)
import Svg.Attributes as SA
import Svg.Elements as SE

type State
  = { nodeData :: NodeData
    , node :: Node
    , selectable :: Boolean
    }

_nodeData :: Lens' State NodeData
_nodeData = prop (SProxy :: SProxy "nodeData")

_position :: Lens' State MathVec2
_position = _nodeData <<< _NodeDataPosition

_stateSelected :: Lens' State Boolean
_stateSelected = _nodeData <<< _NodeDataSelected

_selectable :: Lens' State Boolean
_selectable = prop (SProxy :: _ "selectable")

data Action
  = SetSelection Boolean

data Query a
  = Unselect a
  | GetData (NodeData -> a)
  | Drag (Vec2 Number) a

type ChildSlots
  = ()

type Output
  = Void

type Input
  = { nodeData :: NodeData
    , node :: Node
    , selectable :: Boolean
    }

component :: forall m. MonadEffect m => Component HH.HTML Query Input Output m
component =
  mkComponent
    { initialState: identity
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
    Drag offest inner -> do
      selected <- gets $ view _stateSelected
      when selected $ modify_ $ over _position ((+) offest)
      pure $ Just inner
    -- This runs when the Scene component wants us to save the data
    GetData k -> do
      nodeData <- gets $ view _nodeData
      pure $ Just $ k nodeData

  render ({ selectable, nodeData: NodeData { position, selected } }) =
    SE.g
      [ SA.transform [ SA.Translate (position !! d0) (position !! d1) ]
      ]
      [ SE.rect
          [ SA.fill $ Just $ SA.RGB 255 255 255
          , SA.width $ 100.0
          , SA.height $ 100.0
          , SA.stroke $ Just $ if (selected && selectable) then SA.RGB 118 255 2 else SA.RGB 63 196 255
          , onMouseDown $ const $ if selectable then Just $ SetSelection true else Nothing
          ]
      ]
