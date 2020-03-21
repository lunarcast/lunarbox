module Lunarbox.Component.Editor.Node where

import Prelude
import Data.Lens (Lens', set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown, onMouseUp)
import Lunarbox.Data.NodeData (NodeData(..), _NodeDataSelected)
import Lunarbox.Data.Project (Node)
import Svg.Attributes as SA
import Svg.Elements as SE

type State
  = { nodeData :: NodeData
    , node :: Node
    }

_nodeData :: Lens' State NodeData
_nodeData = prop (SProxy :: SProxy "nodeData")

_stateSelected :: Lens' State Boolean
_stateSelected = _nodeData <<< _NodeDataSelected

data Action
  = SetSelection Boolean

data Query a
  = Void

type ChildSlots
  = ()

type Input
  = Tuple Node NodeData

component :: forall m. MonadEffect m => Component HH.HTML Query Input Void m
component =
  mkComponent
    { initialState: \(Tuple node nodeData) -> { node, nodeData }
    , render
    , eval:
        mkEval
          $ defaultEval
              { handleAction = handleAction
              }
    }
  where
  handleAction :: Action -> HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    SetSelection value -> do
      modify_ $ set _stateSelected value

  render ({ nodeData: NodeData { position, selected } }) =
    SE.rect
      [ SA.width 100.0
      , SA.height 100.0
      , SA.fill $ Just $ SA.RGB 255 255 255
      , SA.x $ position !! d0
      , SA.y $ position !! d1
      , SA.stroke $ Just $ SA.RGB 118 255 2
      , onMouseDown $ const $ Just $ SetSelection true
      , onMouseUp $ const $ Just $ SetSelection false
      ]
