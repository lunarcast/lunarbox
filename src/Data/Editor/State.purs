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
  , deleteNode
  , deleteSelection
  , setRuntimeValue
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
  , _functionUis
  , _ui
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Default (def)
import Data.Editor.Foreign.SceneBoundingBox (getSceneBoundingBox)
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldr)
import Data.Lens (Lens', Traversal', _Just, is, lens, over, preview, set, view)
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
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.Runtime.ValueMap (ValueMap)
import Lunarbox.Data.Dataflow.Type (Type)
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..))
import Lunarbox.Data.Editor.FunctionData (FunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.FunctionUi (FunctionUi)
import Lunarbox.Data.Editor.Location (Location)
import Lunarbox.Data.Editor.Node (Node, _OutputNode, _nodeInput, _nodeInputs)
import Lunarbox.Data.Editor.Node.NodeData (NodeData, _NodeDataSelected)
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
import Lunarbox.Data.Editor.NodeGroup (NodeGroup, _NodeGroupNodes)
import Lunarbox.Data.Editor.PartialConnection (PartialConnection, _from, _to)
import Lunarbox.Data.Editor.Project (Project, _ProjectFunctions, _atProjectFunction, _atProjectNode, _projectNodeGroup, compileProject, createFunction)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Lens (newtypeIso)
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

type State h a
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
    , functionUis :: Map FunctionName (FunctionUi h a)
    }

-- Lenses
_valueMap :: forall h a. Lens' (State h a) (ValueMap Location)
_valueMap = prop (SProxy :: _ "valueMap")

_nodeData :: forall h a. Lens' (State h a) (Map (Tuple FunctionName NodeId) NodeData)
_nodeData = prop (SProxy :: _ "nodeData")

_atNodeData :: forall h a. FunctionName -> NodeId -> Lens' (State h a) (Maybe NodeData)
_atNodeData name id = _nodeData <<< at (Tuple name id)

_functionData :: forall h a. Lens' (State h a) (Map FunctionName FunctionData)
_functionData = prop (SProxy :: _ "functionData")

_atFunctionData :: forall h a. FunctionName -> Lens' (State h a) (Maybe FunctionData)
_atFunctionData name = _functionData <<< at name

_project :: forall h a. Lens' (State h a) Project
_project = prop (SProxy :: _ "project")

_colorMap :: forall h a. Lens' (State h a) (Map Location Color)
_colorMap = prop (SProxy :: _ "colorMap")

_atColorMap :: forall h a. Location -> Traversal' (State h a) (Maybe Color)
_atColorMap location = _colorMap <<< at location

_lastMousePosition :: forall h a. Lens' (State h a) (Maybe (Vec2 Number))
_lastMousePosition = prop (SProxy :: _ "lastMousePosition")

_expression :: forall h a. Lens' (State h a) (Expression Location)
_expression = prop (SProxy :: _ "expression")

_typeMap :: forall h a. Lens' (State h a) (Map Location Type)
_typeMap = prop (SProxy :: _ "typeMap")

_nextId :: forall h a. Lens' (State h a) Int
_nextId = prop (SProxy :: _ "nextId")

_functions :: forall h a. Lens' (State h a) (G.Graph FunctionName DataflowFunction)
_functions = _project <<< _ProjectFunctions

_nodeGroup :: forall h a. FunctionName -> Traversal' (State h a) NodeGroup
_nodeGroup name = _project <<< _projectNodeGroup name

_nodes :: forall h a. FunctionName -> Traversal' (State h a) (G.Graph NodeId Node)
_nodes name = _nodeGroup name <<< _NodeGroupNodes

_atNode :: forall h a. FunctionName -> NodeId -> Traversal' (State h a) (Maybe Node)
_atNode name id = _project <<< _atProjectNode name id

_isSelected :: forall h a. FunctionName -> NodeId -> Traversal' (State h a) Boolean
_isSelected name id = _atNodeData name id <<< _Just <<< _NodeDataSelected

_function :: forall h a. FunctionName -> Traversal' (State h a) (Maybe DataflowFunction)
_function name = _project <<< _atProjectFunction name

_currentFunction :: forall h a. Lens' (State h a) (Maybe FunctionName)
_currentFunction = prop (SProxy :: _ "currentFunction")

_panelIsOpen :: forall h a. Lens' (State h a) Boolean
_panelIsOpen = prop (SProxy :: _ "panelIsOpen")

_currentTab :: forall h a. Lens' (State h a) Tab
_currentTab = prop (SProxy :: _ "currentTab")

_partialConnection :: forall h a. Lens' (State h a) PartialConnection
_partialConnection = prop (SProxy :: _ "partialConnection")

_partialFrom :: forall h a. Lens' (State h a) ((Maybe NodeId))
_partialFrom = _partialConnection <<< _from

_partialTo :: forall h a. Lens' (State h a) (Maybe (Tuple NodeId Int))
_partialTo = _partialConnection <<< _to

_currentNodeGroup :: forall h a. Lens' (State h a) (Maybe NodeGroup)
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

_atCurrentNodeData :: forall h a. NodeId -> Traversal' (State h a) (Maybe NodeData)
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

_currentNodes :: forall h a. Traversal' (State h a) (G.Graph NodeId Node)
_currentNodes = _currentNodeGroup <<< _Just <<< _NodeGroupNodes

_atCurrentNode :: forall h a. NodeId -> Traversal' (State h a) Node
_atCurrentNode id = _currentNodes <<< ix id

_functionUis :: forall h a. Lens' (State h a) (Map FunctionName (FunctionUi h a))
_functionUis = prop (SProxy :: _ "functionUis")

_ui :: forall h a. FunctionName -> Traversal' (State h a) (Maybe (FunctionUi h a))
_ui functionName = _functionUis <<< at functionName

-- Helpers
-- Compile a project
compile :: forall h a. State h a -> State h a
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
tryConnecting :: forall h a. State h a -> State h a
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
setCurrentFunction :: forall h a. Maybe FunctionName -> State h a -> State h a
setCurrentFunction = set _currentFunction

-- Creates a function, adds an output node and set it as the current edited function
initializeFunction :: forall h a. FunctionName -> State h a -> State h a
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
removeConnection :: forall h a. NodeId -> Tuple NodeId Int -> State h a -> State h a
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
setRelativeMousePosition :: forall h a. DOMRect -> Vec2 Number -> State h a -> State h a
setRelativeMousePosition { top, left } position = set _lastMousePosition $ Just $ position - vec2 left top

-- Helper to update the mouse position of the svg scene
getSceneMousePosition :: forall q i o m h a. MonadEffect m => Vec2 Number -> HalogenM (State h a) q i o m (State h a)
getSceneMousePosition position = do
  state <- get
  bounds <- liftEffect getSceneBoundingBox
  pure $ setRelativeMousePosition bounds position state

-- Deletes a node form a given function
deleteNode :: forall h a. FunctionName -> NodeId -> State h a -> State h a
deleteNode functionName id state =
  if isOutput then
    state
  else
    withoutNodeRefs $ removeNodeData $ removeNode state
  where
  node = join $ preview (_atNode functionName id) state

  -- We do not allow deleting output nodes
  isOutput = maybe false (is _OutputNode) node

  nodes = preview (_nodes functionName) state

  withoutNodeRefs =
    over (_nodes functionName) $ map $ over _nodeInputs
      $ map \input ->
          if input == Just id then
            Nothing
          else
            input

  removeNode = over (_nodes functionName) $ G.delete id

  removeNodeData = set (_atNodeData functionName id) Nothing

-- Delete all selected nodes
deleteSelection :: forall h a. State h a -> State h a
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
    pure $ compile $ foldr (deleteNode currentFunction) state selectedNodes

-- Sets the runtime value at a location to any runtime value
setRuntimeValue :: forall h a. FunctionName -> NodeId -> RuntimeValue -> State h a -> State h a
setRuntimeValue functionName nodeId value state =
  set
    (_valueMap <<< newtypeIso <<< at (DeepLocation functionName $ Location nodeId))
    (Just value)
    state
