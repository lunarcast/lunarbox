module Lunarbox.Data.Editor.State
  ( State
  , Tab(..)
  , tabIcon
  , tryConnecting
  , compile
  , setCurrentFunction
  , initializeFunction
  , setRelativeMousePosition
  , getSceneMousePosition
  , removeConnection
  , _valueMap
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
  , _currentNodes
  , _atCurrentNode
  , _currentNodeGroup
  , _nodes
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Default (def)
import Data.Editor.Foreign.SceneBoundingBox (getSceneBoundingBox)
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldr)
import Data.Lens (Lens', Traversal', _Just, lens, over, preview, set, view)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), snd)
import Data.Vec (vec2)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM, get, liftEffect)
import Lunarbox.Control.Monad.Dataflow.Interpreter (InterpreterContext(..), runInterpreter)
import Lunarbox.Control.Monad.Dataflow.Interpreter.Interpret (interpret)
import Lunarbox.Control.Monad.Dataflow.Solve.SolveExpression (solveExpression)
import Lunarbox.Data.Dataflow.Expression (Expression)
import Lunarbox.Data.Dataflow.Runtime.ValueMap (ValueMap)
import Lunarbox.Data.Dataflow.Type (Type)
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..))
import Lunarbox.Data.Editor.FunctionData (FunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Location (Location)
import Lunarbox.Data.Editor.Node (Node, _nodeInput, _nodeInputs)
import Lunarbox.Data.Editor.Node.NodeData (NodeData, _NodeDataSelected)
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
import Lunarbox.Data.Editor.NodeGroup (NodeGroup, _NodeGroupNodes)
import Lunarbox.Data.Editor.PartialConnection (PartialConnection, _from, _to)
import Lunarbox.Data.Editor.Project (Project, _ProjectFunctions, _atProjectFunction, _atProjectNode, _projectNodeGroup, compileProject, createFunction)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Vector (Vec2)
import Svg.Attributes (Color)
import Web.HTML.HTMLElement (DOMRect)

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
    , valueMap :: ValueMap Location
    }

-- Lenses
_valueMap :: Lens' State (ValueMap Location)
_valueMap = prop (SProxy :: _ "valueMap")

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

_nodes :: FunctionName -> Traversal' State (G.Graph NodeId Node)
_nodes name = _nodeGroup name <<< _NodeGroupNodes

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

_atCurrentNodeData :: NodeId -> Traversal' State (Maybe NodeData)
_atCurrentNodeData id =
  lens
    ( \state -> do
        currentFunction <- view _currentFunction state
        view (_atNodeData currentFunction id) state
    )
    ( \state value ->
        fromMaybe state do
          currentFunction <- view _currentFunction state
          pure $ set (_atNodeData currentFunction id) value state
    )

_currentNodes :: Traversal' State (G.Graph NodeId Node)
_currentNodes = _currentNodeGroup <<< _Just <<< _NodeGroupNodes

_atCurrentNode :: NodeId -> Traversal' State Node
_atCurrentNode id = _currentNodes <<< ix id

-- Helpers
-- Compile a project
compile :: State -> State
compile state@{ project, expression, typeMap, valueMap } =
  let
    expression' = compileProject project

    typeMap' =
      -- we only run the type inference algorithm if the expression changed
      if (expression == expression') then
        typeMap
      else case solveExpression expression' of
        Right map -> Map.delete Nowhere map
        -- TODO: make it so this accounts for errors
        Left _ -> mempty

    context =
      InterpreterContext
        { location: Nowhere
        , termEnv: mempty
        }

    valueMap' =
      if (expression == expression') then
        valueMap
      else
        snd $ runInterpreter context
          $ interpret expression'
  in
    state { expression = expression', typeMap = typeMap', valueMap = valueMap' }

-- Tries connecting the pins the user selected
tryConnecting :: State -> State
tryConnecting state =
  fromMaybe state do
    from <- view _partialFrom state
    Tuple toId toIndex <- view _partialTo state
    currentNodeGroup <- preview _currentNodeGroup state
    currentFunction <- view _currentFunction state
    let
      previousConnection =
        join
          $ preview
              ( _atCurrentNode toId
                  <<< _nodeInput toIndex
              )
              state

      state' = case previousConnection of
        Just id -> over _currentNodes (G.removeEdge id toId) state
        Nothing -> state

      state'' = over _currentNodes (G.insertEdge from toId) state'

      state''' =
        set
          ( _atCurrentNode toId
              <<< _nodeInput toIndex
          )
          (Just from)
          state''

      state'''' = set _partialTo Nothing $ set _partialFrom Nothing state'''
    pure $ compile state''''

-- Set the function the user is editing at the moment
setCurrentFunction :: Maybe FunctionName -> State -> State
setCurrentFunction = set _currentFunction

-- Creates a function, adds an output node and set it as the current edited function
initializeFunction :: FunctionName -> State -> State
initializeFunction name state =
  let
    id = NodeId $ show name <> "-output"

    function = createFunction name id

    state' = over _project function state

    state'' = setCurrentFunction (Just name) state'

    state''' = set (_atNodeData name id) (Just def) state''

    state'''' = set (_atFunctionData name) (Just def) state'''
  in
    compile state''''

-- Remove a conenction from the current function
removeConnection :: NodeId -> (Tuple NodeId Int) -> State -> State
removeConnection from (Tuple toId toIndex) state = state''
  where
  state' = set (_atCurrentNode toId <<< _nodeInput toIndex) Nothing state

  toInputs = view (_atCurrentNode toId <<< _nodeInputs) state'

  inputsToSource :: List _
  inputsToSource =
    foldMap
      ( \maybeInput ->
          maybe mempty pure
            $ do
                input <- maybeInput
                guard $ input == from
                pure input
      )
      toInputs

  state'' =
    -- We only remove the connections if there are no dependencies left
    if List.null inputsToSource then
      over _currentNodes (G.removeEdge from toId) state'
    else
      state'

-- Helper function to set the mouse position relative to the svg element
setRelativeMousePosition :: DOMRect -> Vec2 Number -> State -> State
setRelativeMousePosition { top, left } position = set _lastMousePosition $ Just $ position - vec2 left top

-- Helper to update the mouse position of the svg scene
getSceneMousePosition :: forall q i o m. MonadEffect m => Vec2 Number -> HalogenM State q i o m State
getSceneMousePosition position = do
  state <- get
  bounds <- liftEffect getSceneBoundingBox
  pure $ setRelativeMousePosition bounds position state

-- Deletes a node form a given function
deleteNode :: FunctionName -> NodeId -> State -> State
deleteNode functionName id = over (_nodes functionName) $ G.delete id

-- Delete all selected nodes
deleteSelection :: State -> State
deleteSelection state =
  fromMaybe state do
    currentFunction <- view _currentFunction state
    nodes <- preview _currentNodes state
    let
      selectedNodes =
        Set.mapMaybe
          ( \id -> do
              selected <- preview (_isSelected currentFunction id) state
              guard selected
              pure id
          )
          $ G.keys nodes
    pure $ foldr (deleteNode currentFunction) state selectedNodes
