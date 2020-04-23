module Lunarbox.Data.Editor.State
  ( State
  , Tab(..)
  , tabIcon
  , _nodeData
  , _atNodeData
  , _project
  , _colorMap
  , _atColorMap
  , _lastMousePosition
  , _expression
  , _typeMap
  , _nextId
  , _function
  , _functions
  , _nodeGroup
  , _atNode
  , _isSelected
  , _panelIsOpen
  , _currentFunction
  , _currentTab
  , _functionData
  , _atFunctionData
  , _partialConnection
  , _partialFrom
  , _partialTo
  ) where

import Prelude
import Data.Lens (Lens', Traversal', _Just, lens, over, preview, set, view)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Undefined (undefined)
import Lunarbox.Data.Dataflow.Expression (Expression)
import Lunarbox.Data.Dataflow.Type (Type)
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction)
import Lunarbox.Data.Editor.FunctionData (FunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Location (Location)
import Lunarbox.Data.Editor.Node (Node, _nodeInputs)
import Lunarbox.Data.Editor.Node.NodeData (NodeData, _NodeDataSelected)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.NodeGroup (NodeGroup, _NodeGroupNodes)
import Lunarbox.Data.Editor.PartialConnection (PartialConnection, _from, _to)
import Lunarbox.Data.Editor.Project (Project, _ProjectFunctions, _atProjectFunction, _atProjectNode, _projectNodeGroup)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Lens (listToArrayIso)
import Lunarbox.Data.Vector (Vec2)
import Svg.Attributes (Color)

data Tab
  = Settings
  | Add
  | Tree
  | Problems

derive instance eqTab :: Eq Tab

-- Return the icon for a Tab
-- I could use a show instance
-- but this is more explicit I think
tabIcon :: Tab -> String
tabIcon = case _ of
  Settings -> "settings"
  Add -> "add"
  Tree -> "account_tree"
  Problems -> "error"

type State
  = { currentTab :: Tab
    , panelIsOpen :: Boolean
    , project :: Project
    , nextId :: Int
    , currentFunction :: Maybe FunctionName
    , typeMap :: Map Location Type
    , colorMap :: Map Location Color
    , expression :: Expression Location
    , lastMousePosition :: Maybe (Vec2 Number)
    , nodeData :: Map (Tuple FunctionName NodeId) NodeData
    , functionData :: Map FunctionName FunctionData
    , partialConnection :: PartialConnection
    }

-- Lenses
_nodeData :: Lens' State (Map (Tuple FunctionName NodeId) NodeData)
_nodeData = prop (SProxy :: _ "nodeData")

_atNodeData :: FunctionName -> NodeId -> Lens' State (Maybe NodeData)
_atNodeData name id = _nodeData <<< at (Tuple name id)

_functionData :: Lens' State (Map FunctionName FunctionData)
_functionData = prop (SProxy :: _ "functionData")

_atFunctionData :: FunctionName -> Lens' State (Maybe FunctionData)
_atFunctionData name = _functionData <<< at name

_project :: Lens' State Project
_project = prop (SProxy :: _ "project")

_colorMap :: Lens' State (Map Location Color)
_colorMap = prop (SProxy :: _ "colorMap")

_atColorMap :: Location -> Traversal' State (Maybe Color)
_atColorMap location = _colorMap <<< at location

_lastMousePosition :: Lens' State (Maybe (Vec2 Number))
_lastMousePosition = prop (SProxy :: _ "lastMousePosition")

_expression :: Lens' State (Expression Location)
_expression = prop (SProxy :: _ "expression")

_typeMap :: Lens' State (Map Location Type)
_typeMap = prop (SProxy :: _ "typeMap")

_nextId :: Lens' State Int
_nextId = prop (SProxy :: _ "nextId")

_functions :: Lens' State (G.Graph FunctionName DataflowFunction)
_functions = _project <<< _ProjectFunctions

_nodeGroup :: FunctionName -> Traversal' State NodeGroup
_nodeGroup name = _project <<< _projectNodeGroup name

_atNode :: FunctionName -> NodeId -> Traversal' State (Maybe Node)
_atNode name id = _project <<< _atProjectNode name id

_isSelected :: FunctionName -> NodeId -> Traversal' State Boolean
_isSelected name id = _atNodeData name id <<< _Just <<< _NodeDataSelected

_function :: FunctionName -> Traversal' State (Maybe DataflowFunction)
_function name = _project <<< _atProjectFunction name

_currentFunction :: Lens' State (Maybe FunctionName)
_currentFunction = prop (SProxy :: _ "currentFunction")

_panelIsOpen :: Lens' State Boolean
_panelIsOpen = prop (SProxy :: _ "panelIsOpen")

_currentTab :: Lens' State Tab
_currentTab = prop (SProxy :: _ "currentTab")

_partialConnection :: Lens' State PartialConnection
_partialConnection = prop (SProxy :: _ "partialConnection")

_partialFrom :: Lens' State ((Maybe NodeId))
_partialFrom = _partialConnection <<< _from

_partialTo :: Lens' State (Maybe (Tuple NodeId Int))
_partialTo = _partialConnection <<< _to

_currentNodeGroup :: Lens' State (Maybe NodeGroup)
_currentNodeGroup =
  ( lens
      ( \state -> do
          currentFunction <- view _currentFunction state
          preview (_nodeGroup currentFunction) state
      )
      ( \state maybeValue ->
          fromMaybe state do
            value <- maybeValue
            currentFunction <- view _currentFunction state
            pure $ set (_nodeGroup currentFunction) value state
      )
  )

_currentNodes :: Traversal' State (G.Graph NodeId Node)
_currentNodes = _currentNodeGroup <<< _Just <<< _NodeGroupNodes

_atCurrentNode :: NodeId -> Traversal' State (Maybe Node)
_atCurrentNode id = _currentNodes <<< at id

-- Helpers
tryConnecting :: State -> State
tryConnecting state =
  fromMaybe state do
    from <- view _partialFrom state
    Tuple toId toIndex <- view _partialTo state
    currentNodeGroup <- view _currentNodeGroup state
    let
      state' = over _currentNodes (G.insertEdge from toId) state

      state'' =
        set
          ( _atCurrentNode toId
              <<< _Just
              <<< _nodeInputs
              <<< listToArrayIso
              <<< ix toIndex
          )
          (Just from)
          state
    undefined
