module Lunarbox.Component.Editor.Scene where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.State (get, modify_)
import Data.Foldable (traverse_)
import Data.Graph (Graph) as G
import Data.Lens (Lens', _1, _2, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Data.Vec (vec2)
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, Slot, defaultEval, mkComponent, mkEval, query)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown, onMouseUp)
import Lunarbox.Component.Editor.Node as Node
import Lunarbox.Config (Config)
import Lunarbox.Data.Graph (entries) as G
import Lunarbox.Data.NodeData (NodeData)
import Lunarbox.Data.Project (FunctionName, Node, NodeGroup(..), NodeId, Project, _NodeGroup, _nodes)
import Lunarbox.Data.Vector (Vec2)
import Svg.Elements as SE

type State
  = { project :: Project NodeData
    , function :: Tuple FunctionName (NodeGroup NodeData)
    , lastMousePosition :: Maybe (Vec2 Number)
    }

-- Lenses
_lastMousePosition :: Lens' State (Maybe (Vec2 Number))
_lastMousePosition = prop (SProxy :: SProxy "lastMousePosition")

_function :: Lens' State (Tuple FunctionName (NodeGroup NodeData))
_function = prop (SProxy :: SProxy "function")

_functionName :: Lens' State FunctionName
_functionName = _function <<< _1

_functionNodeGroup :: Lens' State (NodeGroup NodeData)
_functionNodeGroup = _function <<< _2

_StateNodes :: Lens' State (G.Graph NodeId (Tuple Node NodeData))
_StateNodes = _functionNodeGroup <<< _NodeGroup <<< _nodes

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
      state <- get
      let
        nodes = view _StateNodes state

        -- we need the type definition for purescript to know what Unfoldable instance to use
        ids :: Array _
        ids = fst <$> (G.entries nodes)
      -- query all nodes to unselect themselves
      ids
        # traverse_ \id -> do
            query (SProxy :: _ "node") id $ Node.Unselect unit

  handleQuery :: forall a. Query a -> HalogenM State Action ChildSlots Output m (Maybe a)
  handleQuery = case _ of
    SelectFunction new -> do
      -- debugging only
      let
        Tuple name _ = new
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

  render { function: Tuple _ (NodeGroup { nodes }) } =
    SE.svg [ onMouseDown $ const $ Just MouseDown, onMouseUp $ const $ Just MouseUp ]
      $ createNodeComponent
      <$> G.entries nodes
