module Lunarbox.Component.Editor.Scene where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.State (get, gets, modify_)
import Data.Foldable (for_, traverse_)
import Data.Int (toNumber)
import Data.Lens (Lens', _1, _2, set, view)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Data.Unfoldable (class Unfoldable)
import Data.Vec (vec2)
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, Slot, defaultEval, mkComponent, mkEval, query, raise, request, tell)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown, onMouseMove, onMouseUp)
import Lunarbox.Component.Editor.Node as Node
import Lunarbox.Config (Config)
import Lunarbox.Data.Dataflow.FunctionName (FunctionName)
import Lunarbox.Data.Dataflow.NodeId (NodeId)
import Lunarbox.Data.FunctionData (FunctionData)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.NodeData (NodeData)
import Lunarbox.Data.Project (Node, NodeGroup(..), Project, VisualFunction, _NodeGroupNodes, _functions)
import Lunarbox.Data.Vector (Vec2)
import Svg.Attributes as SA
import Svg.Elements as SE
import Web.UIEvent.MouseEvent as ME

type State
  = { project :: Project FunctionData NodeData
    , function :: Tuple FunctionName (NodeGroup NodeData)
    , lastMousePosition :: Maybe (Vec2 Number)
    }

-- Lenses
_lastMousePosition :: Lens' State (Maybe (Vec2 Number))
_lastMousePosition = prop (SProxy :: SProxy "lastMousePosition")

_project :: Lens' State (Project FunctionData NodeData)
_project = prop (SProxy :: _ "project")

_projectFunctions :: Lens' State (G.Graph FunctionName (Tuple (VisualFunction NodeData) FunctionData))
_projectFunctions = _project <<< _functions

_function :: Lens' State (Tuple FunctionName (NodeGroup NodeData))
_function = prop (SProxy :: SProxy "function")

_functionName :: Lens' State FunctionName
_functionName = _function <<< _1

_functionNodeGroup :: Lens' State (NodeGroup NodeData)
_functionNodeGroup = _function <<< _2

_StateNodes :: Lens' State (G.Graph NodeId (Tuple Node NodeData))
_StateNodes = _functionNodeGroup <<< _NodeGroupNodes

data Action
  = MouseMove (Vec2 Number)
  | MouseDown (Vec2 Number)
  | MouseUp
  | Receive Input

data Query a
  = BeforeFunctionChanging a

data Output
  = SaveNodeGroup (NodeGroup NodeData)

type ChildSlots
  = ( node :: Slot Node.Query Void NodeId )

type Input
  = { project :: Project FunctionData NodeData
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
              , receive = Just <<< Receive
              }
    }
  where
  getNodeIds :: forall u. Unfoldable u => Functor u => HalogenM State Action ChildSlots Output m (u NodeId)
  getNodeIds = (map fst) <$> G.toUnfoldable <$> view _StateNodes <$> get

  isDragging :: HalogenM State Action ChildSlots Output m Boolean
  isDragging = Maybe.isJust <$> view _lastMousePosition <$> get

  whenDragging :: HalogenM State Action ChildSlots Output m Unit -> HalogenM State Action ChildSlots Output m Unit
  whenDragging m = isDragging >>= (flip when $ m)

  handleAction :: Action -> HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    Receive { project, function } -> do
      modify_ _ { project = project, function = function }
    MouseMove newPosition ->
      whenDragging do
        maybeOldPosition <- gets $ view _lastMousePosition
        -- the type definition is necessary to tell purescript what unfoldable instance to use
        ids :: Array _ <- getNodeIds
        -- remember the current mouse position
        modify_ $ set _lastMousePosition $ Just newPosition
        -- each node will move itself if it knows it's selected
        for_ ids \id ->
          for_ maybeOldPosition \oldPosition ->
            query (SProxy :: _ "node") id $ tell $ Node.Drag $ newPosition - oldPosition
    MouseDown position -> do
      modify_ $ set _lastMousePosition $ Just position
    MouseUp -> do
      (ids :: Array _) <- getNodeIds
      -- forget mouse position
      modify_ $ set _lastMousePosition Nothing
      -- query all nodes to unselect themselves
      for_ ids \id -> do
        mousePosition <- gets $ view _lastMousePosition
        query (SProxy :: _ "node") id $ tell Node.Unselect

  saveCurrentNodeGroup :: HalogenM State Action ChildSlots Output m Unit
  saveCurrentNodeGroup = getGroup >>= emitGroup
    where
    getGroup = gets $ view _functionNodeGroup

    emitGroup = raise <<< SaveNodeGroup

  handleQuery :: forall a. Query a -> HalogenM State Action ChildSlots Output m (Maybe a)
  handleQuery = case _ of
    BeforeFunctionChanging k -> do
      -- this chunk is here to save all nodes
      (ids :: Array _) <- getNodeIds
      -- query all nodes to unselect themselves
      for_ ids \id ->
        do
          query (SProxy :: _ "node") id $ request Node.GetData
          >>= traverse_
              ( modify_
                  <<< set (_StateNodes <<< ix id <<< _2)
              )
      -- save the edits we made to the node group
      saveCurrentNodeGroup
      pure $ Just k

  createNodeComponent :: Tuple NodeId (Tuple Node NodeData) -> HTML _ Action
  createNodeComponent (Tuple id (Tuple node nodeData)) =
    HH.slot
      (SProxy :: _ "node")
      id
      Node.component
      { node, nodeData, selectable: true }
      absurd

  render { function: Tuple _ (NodeGroup { nodes }) } =
    SE.svg
      [ SA.width 100000.0
      , SA.height 100000.0
      , onMouseMove $ \e -> Just $ MouseMove $ toNumber <$> vec2 (ME.pageX e) (ME.pageY e)
      , onMouseDown $ \e -> Just $ MouseDown $ toNumber <$> vec2 (ME.pageX e) (ME.pageY e)
      , onMouseUp $ const $ Just MouseUp
      ]
      $ createNodeComponent
      <$> G.toUnfoldable nodes
