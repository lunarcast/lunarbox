module Lunarbox.Data.Editor.State where

import Prelude
import Control.Monad.State (gets)
import Control.Monad.State as StateM
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Default (def)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (foldMap, foldr, for_, traverse_)
import Data.Int (toNumber)
import Data.Lens (Lens', Traversal', _Just, is, lens, over, preview, set, view)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.List (List, (..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Data.Unfoldable (replicate)
import Data.Vec (vec2)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM, RefLabel(..), get, getHTMLElementRef, liftEffect, modify_)
import Lunarbox.Control.Monad.Dataflow.Interpreter (InterpreterContext(..), runInterpreter)
import Lunarbox.Control.Monad.Dataflow.Interpreter.Interpret (interpret)
import Lunarbox.Control.Monad.Dataflow.Solve.SolveExpression (solveExpression)
import Lunarbox.Control.Monad.Dataflow.Solve.Unify (canUnify)
import Lunarbox.Data.Dataflow.Expression (Expression)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.Runtime.ValueMap (ValueMap)
import Lunarbox.Data.Dataflow.Type (Type, inputs)
import Lunarbox.Data.Editor.Camera (Camera, toWorldCoordinates)
import Lunarbox.Data.Editor.Camera as Camera
import Lunarbox.Data.Editor.Constants (nodeOffset, nodeOffsetGrowthRate, nodeOffsetInitialRadius)
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction, _VisualFunction)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..), nothing)
import Lunarbox.Data.Editor.Foreign.NodeBoundingdBox (nodeBoundingBox)
import Lunarbox.Data.Editor.FunctionData (FunctionData, _FunctionDataInputs, internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.FunctionUi (FunctionUi)
import Lunarbox.Data.Editor.Location (Location)
import Lunarbox.Data.Editor.Node (Node(..), _OutputNode, _nodeInput, _nodeInputs)
import Lunarbox.Data.Editor.Node.NodeData (NodeData, _NodeDataPosition, _NodeDataSelected)
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Editor.NodeGroup (NodeGroup, _NodeGroupInputs, _NodeGroupNodes, _NodeGroupOutput)
import Lunarbox.Data.Editor.PartialConnection (PartialConnection, _from, _to)
import Lunarbox.Data.Editor.Project (Project(..), _ProjectFunctions, _atProjectFunction, _atProjectNode, _projectNodeGroup, compileProject, createFunction)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Lens (newtypeIso)
import Lunarbox.Data.Math (polarToCartesian)
import Lunarbox.Data.Vector (Vec2)
import Math (pow)
import Svg.Attributes (Color)
import Web.DOM.Node as WebNode
import Web.HTML.HTMLElement (DOMRect, toNode)

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

type State a s m
  = { currentTab :: Tab
    , panelIsOpen :: Boolean
    , project :: Project
    , nextId :: Int
    , currentFunction :: Maybe FunctionName
    , typeMap :: Map Location Type
    , colorMap :: Map Location Color
    , expression :: Expression Location
    , lastMousePosition :: Vec2 Number
    , nodeData :: Map (Tuple FunctionName NodeId) NodeData
    , functionData :: Map FunctionName FunctionData
    , partialConnection :: PartialConnection
    , valueMap :: ValueMap Location
    , functionUis :: Map FunctionName (FunctionUi a s m)
    , runtimeOverwrites :: ValueMap Location
    , cameras :: Map FunctionName Camera
    , sceneScale :: Vec2 Number
    , addedNodes :: Int
    , inputCountMap :: Map FunctionName Int
    , unconnectablePins :: Set.Set (ExtendedLocation NodeId Pin)
    , name :: String
    , example :: Boolean
    }

-- Starting state which contains nothing
emptyState :: forall a s m. State a s m
emptyState =
  initializeFunction (FunctionName "main")
    { currentTab: Settings
    , currentFunction: Nothing
    , nextId: 0
    , addedNodes: 0
    , panelIsOpen: false
    , typeMap: mempty
    , colorMap: mempty
    , functionData: mempty
    , valueMap: mempty
    , runtimeOverwrites: mempty
    , functionUis: mempty
    , cameras: mempty
    , nodeData: mempty
    , inputCountMap: mempty
    , unconnectablePins: mempty
    , partialConnection: def
    , sceneScale: zero
    , lastMousePosition: zero
    , expression: nothing
    , project: Project { main: FunctionName "main", functions: G.emptyGraph }
    , name: "Unnamed project"
    , example: false
    }

-- Helpers
-- Generate an id and icncrease the inner counter in the state
createId :: forall a s m. StateM.State (State a s m) NodeId
createId = do
  nextId <- gets $ view _nextId
  modify_ $ over _nextId (_ + 1)
  pure $ NodeId $ show nextId

inputNodeName :: FunctionName
inputNodeName = FunctionName "input"

createNode :: forall a s m. FunctionName -> StateM.State (State a s m) Unit
createNode name = do
  let
    isInput = name == inputNodeName
  id <- createId
  maybeCurrentFunction <- gets $ view _currentFunction
  desiredInputCount <- gets $ preview $ _atInputCount name
  functionDataInputs <-
    if isInput then
      pure []
    else
      gets
        $ view (_atFunctionData name <<< _Just <<< _FunctionDataInputs)
  addedNodes <- gets $ (toNumber <<< view _addedNodes)
  center <- gets sceneCenter
  let
    node =
      if isInput then
        InputNode
      else
        ComplexNode
          { inputs: replicate inputCount Nothing
          , function: name
          }
      where
      maxInputs = Array.length functionDataInputs

      inputCount = fromMaybe maxInputs $ join desiredInputCount

      inputs = (DeepLocation name <<< DeepLocation id <<< InputPin) <$> 0 .. (inputCount - 1)

    angle = nodeOffset * addedNodes

    radius = nodeOffsetInitialRadius * (nodeOffsetGrowthRate `pow` angle)

    offset = polarToCartesian radius angle

    position = offset + center

    nodeData = set _NodeDataPosition position def
  for_ maybeCurrentFunction
    $ \currentFunction -> do
        modify_ $ over _addedNodes (_ + 1)
        modify_ $ set (_atNode currentFunction id) $ Just node
        modify_ $ set (_atNodeData currentFunction id) $ Just nodeData
        when isInput $ modify_ $ over (_currentNodeGroup <<< _Just <<< _NodeGroupInputs) $ (_ <> pure id)
        when (not isInput) $ modify_ $ over _functions $ G.insertEdge name currentFunction
        modify_ $ compile

-- Get the type of the output a node (Also works for input pins)
getOutputType :: forall a s m. FunctionName -> NodeId -> State a s m -> Maybe Type
getOutputType functionName id state = do
  let
    typeMap = view _typeMap state
  nodeGroup <- preview (_nodeGroup functionName) state
  currentFunctionType <- Map.lookup (Location functionName) typeMap
  let
    inputIndex = List.findIndex (_ == id) $ view _NodeGroupInputs nodeGroup
  case inputIndex of
    Just index -> (inputs currentFunctionType) `List.index` index
    Nothing -> Map.lookup (DeepLocation functionName $ DeepLocation id OutputPin) typeMap

-- Get all the input pins in the current function
currentInputSet :: forall a s m. State a s m -> Set.Set (Tuple NodeId Int)
currentInputSet state =
  let
    nodeGroup = fromMaybe G.emptyGraph $ preview _currentNodes state
  in
    Set.fromFoldable
      $ ( \(Tuple id node) ->
            let
              inputs = view _nodeInputs node
            in
              List.mapWithIndex (\index -> const $ Tuple id index) inputs
        )
      =<< G.toUnfoldable nodeGroup

-- Ger a list of all the outputs
currentOutputList :: forall a s m. State a s m -> Set.Set NodeId
currentOutputList state =
  let
    nodes = fromMaybe G.emptyGraph $ preview _currentNodes state

    output = preview (_currentNodeGroup <<< _Just <<< _NodeGroupOutput) state

    keys = G.keys nodes
  in
    case output of
      Just id -> Set.difference keys $ Set.singleton id
      Nothing -> keys

-- Generate the list of inputs can't be connected 
generateUnconnectableInputs :: forall a s m. NodeId -> State a s m -> State a s m
generateUnconnectableInputs output state =
  let
    inputs = currentInputSet state

    unconnectableInputs = Set.filter (not <<< flip (canConnect output) state) inputs

    locations = (\(Tuple nodeId index) -> DeepLocation nodeId $ InputPin index) `Set.map` unconnectableInputs
  in
    set _unconnectablePins locations state

-- Generates a list of outputs which can't be connected
generateUnconnectableOutputs :: forall a s m. Tuple NodeId Int -> State a s m -> State a s m
generateUnconnectableOutputs input state =
  let
    outputs = currentOutputList state

    unconnectableOutputs = Set.filter (\outputId -> not $ canConnect outputId input state) outputs

    locations = (\outputId -> DeepLocation outputId OutputPin) `Set.map` unconnectableOutputs
  in
    set _unconnectablePins locations state

-- Make a list with everything which cannot be connected
makeUnconnetacbleList :: forall a s m. State a s m -> State a s m
makeUnconnetacbleList state = case view _partialFrom state of
  Just from -> generateUnconnectableInputs from state
  Nothing -> case view _partialTo state of
    Just to -> generateUnconnectableOutputs to state
    Nothing -> set _unconnectablePins mempty state

-- Compile a project
compile :: forall a s m. State a s m -> State a s m
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

    visualFunctions :: List FunctionName
    visualFunctions =
      Set.toUnfoldable
        $ Map.keys
        $ Map.filter (is _VisualFunction)
        $ G.toMap
        $ view _functions state

    state' =
      foldr
        ( \functionName state'' ->
            fromMaybe state'' do
              functionType <- Map.lookup (Location functionName) typeMap
              let
                functionData =
                  internal
                    $ List.toUnfoldable
                    $ List.mapWithIndex (\index _ -> { name: "Input " <> show index })
                    $ inputs functionType
              pure $ set (_atFunctionData functionName) (Just functionData) state''
        )
        state
        visualFunctions
  in
    evaluate
      $ makeUnconnetacbleList
      $ state' { expression = expression', typeMap = typeMap' }

-- Evaluate the current expression and write into the value map
evaluate :: forall a s m. State a s m -> State a s m
evaluate state = set _valueMap valueMap state
  where
  context =
    InterpreterContext
      { location: Nowhere
      , termEnv: mempty
      , overwrites: view _runtimeOverwrites state
      }

  expression = view _expression state

  valueMap =
    snd $ runInterpreter context
      $ interpret expression

-- Check if 2 pins can be connected
canConnect :: forall a s m. NodeId -> Tuple NodeId Int -> State a s m -> Boolean
canConnect from (Tuple toId toIndex) state =
  fromMaybe false do
    let
      typeMap = view _typeMap state
    nodes <- preview _currentNodes state
    guard $ not $ G.wouldCreateCycle from toId nodes
    currentFunction <- view _currentFunction state
    fromType <- getOutputType currentFunction from state
    toType <- Map.lookup (DeepLocation currentFunction $ DeepLocation toId $ InputPin toIndex) typeMap
    guard $ canUnify toType fromType
    pure true

-- Tries connecting the pins the user selected
tryConnecting :: forall a s m. State a s m -> State a s m
tryConnecting state =
  fromMaybe state do
    let
      typeMap = view _typeMap state
    from <- view _partialFrom state
    Tuple toId toIndex <- view _partialTo state
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
setCurrentFunction :: forall a s m. Maybe FunctionName -> State a s m -> State a s m
setCurrentFunction name = makeUnconnetacbleList <<< set _currentFunction name

-- Creates a function, adds an output node and set it as the current edited function
initializeFunction :: forall a s m. FunctionName -> State a s m -> State a s m
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
removeConnection :: forall a s m. NodeId -> Tuple NodeId Int -> State a s m -> State a s m
removeConnection from (Tuple toId toIndex) state = compile state''
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
setRelativeMousePosition :: forall a s m. DOMRect -> Vec2 Number -> State a s m -> State a s m
setRelativeMousePosition { top, left } position = set _lastMousePosition sceneCoordinates
  where
  sceneCoordinates = position - vec2 left top

-- Get the domRect representation of the bounding box of the scene element
getSceneBoundingBox :: forall q i o a s m. MonadEffect m => HalogenM (State a s m) q i o m (Maybe DOMRect)
getSceneBoundingBox = do
  elem <- getHTMLElementRef sceneRef
  sceneNode <- traverse (liftEffect <<< WebNode.firstChild <<< toNode) elem
  pure $ nodeBoundingBox <$> join sceneNode

-- Helper to update the mouse position of the svg scene
getSceneMousePosition :: forall q i o a s m. MonadEffect m => Vec2 Number -> HalogenM (State a s m) q i o m (State a s m)
getSceneMousePosition position = do
  state <- get
  maybeBounds <- getSceneBoundingBox
  pure $ maybe state (\bounds -> setRelativeMousePosition bounds position state) maybeBounds

-- Deletes a node form a given function
deleteNode :: forall a s m. FunctionName -> NodeId -> State a s m -> State a s m
deleteNode functionName id state =
  if isOutput then
    state
  else
    withoutInput $ withoutNodeRefs $ removeNodeData $ removeNode state
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

  withoutInput = over (_currentNodeGroup <<< _Just <<< _NodeGroupInputs) $ filter (id == _)

-- Delete all selected nodes
deleteSelection :: forall a s m. State a s m -> State a s m
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
setRuntimeValue :: forall a s m. FunctionName -> NodeId -> RuntimeValue -> State a s m -> State a s m
setRuntimeValue functionName nodeId value =
  evaluate
    <<< set
        (_runtimeOverwrites <<< newtypeIso <<< at (DeepLocation functionName $ Location nodeId))
        (Just value)

-- This makes the node start from the middle again
resetNodeOffset :: forall a s m. State a s m -> State a s m
resetNodeOffset = set _addedNodes 0

-- Set the scale of the scene
setScale :: forall a s m. DOMRect -> State a s m -> State a s m
setScale { height, width } = resetNodeOffset <<< (set _sceneScale $ vec2 width height)

-- Ref to access the scene svg
sceneRef :: RefLabel
sceneRef = RefLabel "scene"

-- Adjusts the scale based on the scene I get in ts
adjustSceneScale :: forall q i o m a s. MonadEffect m => HalogenM (State a s m) q i o m Unit
adjustSceneScale = getSceneBoundingBox >>= traverse_ (modify_ <<< setScale)

-- Pan the current camera in  screen coordinates
pan :: forall a s m. Vec2 Number -> State a s m -> State a s m
pan = over _currentCamera <<< Camera.pan

-- Get the coordinates of the center of the scene in world coordinates
sceneCenter :: forall a s m. State a s m -> Vec2 Number
sceneCenter state = toWorldCoordinates camera $ (_ / 2.0) <$> scale
  where
  scale = view _sceneScale state

  camera = view _currentCamera state

-- Lenses
_inputCountMap :: forall a s m. Lens' (State a s m) (Map FunctionName Int)
_inputCountMap = prop (SProxy :: _ "inputCountMap")

_atInputCount :: forall a s m. FunctionName -> Traversal' (State a s m) (Maybe Int)
_atInputCount name = _inputCountMap <<< at name

_addedNodes :: forall a s m. Lens' (State a s m) Int
_addedNodes = prop (SProxy :: _ "addedNodes")

_cameras :: forall a s m. Lens' (State a s m) (Map FunctionName Camera)
_cameras = prop (SProxy :: _ "cameras")

_camera :: forall a s m. FunctionName -> Traversal' (State a s m) (Maybe Camera)
_camera name = _cameras <<< at name

_sceneScale :: forall a s m. Lens' (State a s m) (Vec2 Number)
_sceneScale = prop (SProxy :: _ "sceneScale")

_runtimeOverwrites :: forall a s m. Lens' (State a s m) (ValueMap Location)
_runtimeOverwrites = prop (SProxy :: _ "runtimeOverwrites")

_valueMap :: forall a s m. Lens' (State a s m) (ValueMap Location)
_valueMap = prop (SProxy :: _ "valueMap")

_nodeData :: forall a s m. Lens' (State a s m) (Map (Tuple FunctionName NodeId) NodeData)
_nodeData = prop (SProxy :: _ "nodeData")

_atNodeData :: forall a s m. FunctionName -> NodeId -> Lens' (State a s m) (Maybe NodeData)
_atNodeData name id = _nodeData <<< at (Tuple name id)

_functionData :: forall a s m. Lens' (State a s m) (Map FunctionName FunctionData)
_functionData = prop (SProxy :: _ "functionData")

_atFunctionData :: forall a s m. FunctionName -> Lens' (State a s m) (Maybe FunctionData)
_atFunctionData name = _functionData <<< at name

_project :: forall a s m. Lens' (State a s m) Project
_project = prop (SProxy :: _ "project")

_colorMap :: forall a s m. Lens' (State a s m) (Map Location Color)
_colorMap = prop (SProxy :: _ "colorMap")

_atColorMap :: forall a s m. Location -> Traversal' (State a s m) (Maybe Color)
_atColorMap location = _colorMap <<< at location

_lastMousePosition :: forall a s m. Lens' (State a s m) (Vec2 Number)
_lastMousePosition = prop (SProxy :: _ "lastMousePosition")

_expression :: forall a s m. Lens' (State a s m) (Expression Location)
_expression = prop (SProxy :: _ "expression")

_typeMap :: forall a s m. Lens' (State a s m) (Map Location Type)
_typeMap = prop (SProxy :: _ "typeMap")

_nextId :: forall a s m. Lens' (State a s m) Int
_nextId = prop (SProxy :: _ "nextId")

_functions :: forall a s m. Lens' (State a s m) (G.Graph FunctionName DataflowFunction)
_functions = _project <<< _ProjectFunctions

_nodeGroup :: forall a s m. FunctionName -> Traversal' (State a s m) NodeGroup
_nodeGroup name = _project <<< _projectNodeGroup name

_nodes :: forall a s m. FunctionName -> Traversal' (State a s m) (G.Graph NodeId Node)
_nodes name = _nodeGroup name <<< _NodeGroupNodes

_atNode :: forall a s m. FunctionName -> NodeId -> Traversal' (State a s m) (Maybe Node)
_atNode name id = _project <<< _atProjectNode name id

_isSelected :: forall a s m. FunctionName -> NodeId -> Traversal' (State a s m) Boolean
_isSelected name id = _atNodeData name id <<< _Just <<< _NodeDataSelected

_function :: forall a s m. FunctionName -> Traversal' (State a s m) (Maybe DataflowFunction)
_function name = _project <<< _atProjectFunction name

_currentFunction :: forall a s m. Lens' (State a s m) (Maybe FunctionName)
_currentFunction = prop (SProxy :: _ "currentFunction")

_panelIsOpen :: forall a s m. Lens' (State a s m) Boolean
_panelIsOpen = prop (SProxy :: _ "panelIsOpen")

_currentTab :: forall a s m. Lens' (State a s m) Tab
_currentTab = prop (SProxy :: _ "currentTab")

_partialConnection :: forall a s m. Lens' (State a s m) PartialConnection
_partialConnection = prop (SProxy :: _ "partialConnection")

_partialFrom :: forall a s m. Lens' (State a s m) ((Maybe NodeId))
_partialFrom = _partialConnection <<< _from

_partialTo :: forall a s m. Lens' (State a s m) (Maybe (Tuple NodeId Int))
_partialTo = _partialConnection <<< _to

_currentNodeGroup :: forall a s m. Lens' (State a s m) (Maybe NodeGroup)
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

_atCurrentNodeData :: forall a s m. NodeId -> Traversal' (State a s m) (Maybe NodeData)
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

_currentNodes :: forall a s m. Traversal' (State a s m) (G.Graph NodeId Node)
_currentNodes = _currentNodeGroup <<< _Just <<< _NodeGroupNodes

_atCurrentNode :: forall a s m. NodeId -> Traversal' (State a s m) Node
_atCurrentNode id = _currentNodes <<< ix id

_functionUis :: forall a s m. Lens' (State a s m) (Map FunctionName (FunctionUi a s m))
_functionUis = prop (SProxy :: _ "functionUis")

_ui :: forall a s m. FunctionName -> Traversal' (State a s m) (Maybe (FunctionUi a s m))
_ui functionName = _functionUis <<< at functionName

_currentCamera :: forall a s m. Lens' (State a s m) Camera
_currentCamera =
  lens
    ( \state ->
        fromMaybe def do
          currentFunction <- view _currentFunction state
          join $ preview (_camera currentFunction) state
    )
    ( \state value ->
        fromMaybe state do
          currentFunction <- view _currentFunction state
          pure $ set (_camera currentFunction) (Just value) state
    )

_unconnectablePins :: forall a s m. Lens' (State a s m) (Set.Set (ExtendedLocation NodeId Pin))
_unconnectablePins = prop (SProxy :: _ "unconnectablePins")
