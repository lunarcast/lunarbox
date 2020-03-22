module Lunarbox.Component.Editor.Scene where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.State (modify_)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Vec (vec2)
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, Slot, defaultEval, mkComponent, mkEval)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown)
import Lunarbox.Component.Editor.Node as Node
import Lunarbox.Config (Config)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.NodeData (NodeData)
import Lunarbox.Data.Project (FunctionName, Node, NodeGroup(..), NodeId, Project)
import Lunarbox.Data.Vector (Vec2)
import Lunarbox.Control.Monad.Effect (print)
import Svg.Elements as SE

type State
  = { project :: Project NodeData
    , function :: Tuple FunctionName (NodeGroup NodeData)
    , lastMousePosition :: Maybe (Vec2 Number)
    }

-- Lenses
_lastMousePosition :: Lens' State (Maybe (Vec2 Number))
_lastMousePosition = prop (SProxy :: SProxy "lastMousePosition")

data Action
  = MouseMove
  | MouseDown
  | MouseUp

data Query a
  = SelectFunction (Tuple FunctionName (NodeGroup NodeData))

type Output
  = Void

type ChildSlots
  = ( node :: Slot Node.Query Void NodeId )

type Input
  = { project :: Project NodeData
    , function :: Tuple FunctionName (NodeGroup NodeData)
    }

component :: forall m. MonadEffect m => MonadAsk Config m => Component HH.HTML Query Input Output m
component =
  mkComponent
    { initialState:
        \{ project, function } ->
          { project, function, lastMousePosition: Nothing
          }
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
    MouseMove -> do
      pure unit
    MouseDown -> do
      modify_ (_ { lastMousePosition = Just $ vec2 0.0 0.0 })
    MouseUp -> do
      -- forget mouse position
      modify_ (_ { lastMousePosition = Nothing })

  handleQuery :: forall a. Query a -> HalogenM State Action ChildSlots Output m (Maybe a)
  handleQuery = case _ of
    SelectFunction new -> do
      -- debugging only
      let
        Tuple name _ = new
      print name
      modify_ (_ { function = new })
      pure Nothing

  createNodeComponent :: Tuple NodeId (Tuple Node NodeData) -> HTML _ Action
  createNodeComponent (Tuple id input) =
    HH.slot
      (SProxy :: _ "node")
      id
      Node.component
      input
      absurd

  render { project, function: Tuple _ (NodeGroup { nodes }) } =
    SE.svg [ onMouseDown $ const $ Just MouseDown ]
      $ createNodeComponent
      <$> G.entries nodes
