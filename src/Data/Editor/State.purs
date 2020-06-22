module Lunarbox.Data.Editor.State where

import Prelude
import Control.Monad.State (execState, gets, put, runState)
import Control.Monad.State as StateM
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Default (def)
import Data.Either (Either(..))
import Data.Filterable (filter, filterMap)
import Data.Foldable (foldMap, foldr, length, traverse_)
import Data.Lens (Lens', Traversal', _Just, is, lens, over, preview, set, view)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Newtype as Newtype
import Data.Nullable as Nullable
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Unfoldable (replicate)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM, liftEffect, modify_)
import Lunarbox.Capability.Editor.Type (generateColorMap)
import Lunarbox.Control.Monad.Dataflow.Interpreter (InterpreterContext(..), runInterpreter)
import Lunarbox.Control.Monad.Dataflow.Interpreter.Interpret (interpret)
import Lunarbox.Control.Monad.Dataflow.Solve.SolveExpression (solveExpression)
import Lunarbox.Control.Monad.Dataflow.Solve.Unify (canUnify)
import Lunarbox.Data.Dataflow.Expression (Expression)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.Runtime.ValueMap (ValueMap(..))
import Lunarbox.Data.Dataflow.Type (Type, inputs)
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction(..), _VisualFunction)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..), _ExtendedLocation, nothing)
import Lunarbox.Data.Editor.FunctionData (FunctionData, _FunctionDataInputs, internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.FunctionUi (FunctionUi)
import Lunarbox.Data.Editor.Location (Location)
import Lunarbox.Data.Editor.Node (Node(..), _OutputNode, _nodeInput, _nodeInputs, getFunctionName)
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Editor.NodeGroup (NodeGroup(..), _NodeGroupInputs, _NodeGroupNodes, _NodeGroupOutput)
import Lunarbox.Data.Editor.PartialConnection (PartialConnection, _from, _to)
import Lunarbox.Data.Editor.Project (Project(..), _ProjectFunctions, _atProjectFunction, _atProjectNode, _projectNodeGroup, compileProject, createFunction)
import Lunarbox.Data.Graph (emptyGraph)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Lens (newtypeIso)
import Lunarbox.Data.Ord (sortBySearch)
import Lunarbox.Foreign.Render (GeometryCache)
import Lunarbox.Foreign.Render as Native
import Svg.Attributes (Color)
import Svg.Attributes as Color
import Web.Event.Event as Event
import Web.Event.Internal.Types (Event)

data Tab
  = Settings
  | Add
  | Tree

derive instance eqTab :: Eq Tab

-- Return the icon for a Tab
-- I could use a show instance
-- but this is more explicit I think
tabIcon :: Tab -> String
tabIcon = case _ of
  Settings -> "settings"
  Add -> "add"
  Tree -> "account_tree"

type State a s m
  = { currentTab :: Tab
    , panelIsOpen :: Boolean
    , project :: Project
    , nextId :: Int
    , currentFunction :: Maybe FunctionName
    , typeMap :: Map Location Type
    , colorMap :: Map Location Color
    , expression :: Expression Location
    , geometries :: Map FunctionName GeometryCache
    , functionData :: Map FunctionName FunctionData
    , partialConnection :: PartialConnection
    , valueMap :: ValueMap Location
    , functionUis :: Map FunctionName (FunctionUi a s m)
    , runtimeOverwrites :: ValueMap Location
    , inputCountMap :: Map FunctionName Int
    , unconnectablePins :: Set.Set (ExtendedLocation NodeId Pin)
    , name :: String
    , isExample :: Boolean
    , isAdmin :: Boolean
    , nodeSearchTerm :: String
    , isVisible :: Boolean
    }

-- Starting state which contains nothing
emptyState :: forall a s m. State a s m
emptyState =
  initializeFunction (FunctionName "main")
    { currentTab: Settings
    , currentFunction: Nothing
    , nextId: 0
    , panelIsOpen: false
    , typeMap: mempty
    , colorMap: mempty
    , functionData: mempty
    , valueMap: mempty
    , runtimeOverwrites: mempty
    , functionUis: mempty
    , inputCountMap: mempty
    , unconnectablePins: mempty
    , geometries: mempty
    , partialConnection: def
    , expression: nothing
    , project: Project { main: FunctionName "main", functions: mempty }
    , name: "Unnamed project"
    , nodeSearchTerm: ""
    , isExample: false
    , isAdmin: false
    , isVisible: false
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

-- 
-- Update the way a node looks to fit the latest data
updateNode :: forall q i o m a s. MonadEffect m => NodeId -> HalogenM (State a s m) q i o m Unit
updateNode id =
  gets (view _currentGeometryCache)
    >>= traverse_ \cache ->
        gets (view _currentFunction)
          >>= traverse_ \currentFunction ->
              gets (preview $ _atCurrentNode id)
                >>= traverse_ \node -> do
                    typeMap <- gets $ view _typeMap
                    let
                      inputs = List.toUnfoldable $ view _nodeInputs node

                      colorMap = generateColorMap (\pin -> flip Map.lookup typeMap $ DeepLocation currentFunction $ DeepLocation id pin) node
                    liftEffect
                      $ Native.refreshInputArcs cache id
                          { inputs: Nullable.toNullable <$> inputs
                          , colorMap:
                            foldr (uncurry go)
                              { output: Nullable.null
                              , inputs: replicate (length inputs) Nullable.null
                              }
                              $ (Map.toUnfoldable colorMap :: Array _)
                          }
  where
  -- TODO: remove this once I get rid of this trash color system from halogen-svg
  showColor (Color.RGB r g b) = "rgb(" <> show r <> "," <> show g <> "," <> show b <> ")"

  showColor (Color.RGBA r g b a) = "rgba(" <> show r <> "," <> show g <> "," <> show b <> "," <> show a <> ")"

  go OutputPin color map = map { output = Nullable.notNull $ showColor color }

  go (InputPin index) color map =
    map
      { inputs =
        fromMaybe map.inputs
          $ Array.updateAt index (Nullable.notNull $ showColor color) map.inputs
      }

--
-- Create a new node and save all the data in the different required places
createNode :: forall q i o m a s. MonadEffect m => FunctionName -> HalogenM (State a s m) q i o m Unit
createNode name = do
  let
    create = do
      let
        isInput = name == inputNodeName
      id <- createId
      desiredInputCount <- gets $ preview $ _atInputCount name
      functionDataInputs <-
        if isInput then
          pure []
        else
          gets
            $ view (_atFunctionData name <<< _Just <<< _FunctionDataInputs)
      let
        inputCount = fromMaybe (length functionDataInputs) $ join desiredInputCount

        node =
          if isInput then
            InputNode
          else
            ComplexNode
              { inputs: replicate inputCount Nothing
              , function: name
              }
      gets (view _currentFunction)
        >>= traverse_ \currentFunction -> do
            modify_ $ set (_atNode currentFunction id) $ Just node
            when isInput $ modify_ $ over (_currentNodeGroup <<< _Just <<< _NodeGroupInputs) $ (_ <> pure id)
            modify_ compile
      pure $ Tuple id inputCount
  Tuple (Tuple id inputs) newState <- gets $ runState create
  gets (view _currentGeometryCache)
    >>= traverse_ \cache -> do
        liftEffect $ Native.createNode cache id inputs
        void $ put newState
        updateNode id

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
        -- WARNING: in case of errors we just return an empty typemap
        -- This is not a good idea since in theory this "could" break
        -- even tho in practice we never get to this point...
        -- It would still be useful to do something with the error message
        -- for debugging or stuff like that
        Left _ -> mempty

    visualFunctions :: List FunctionName
    visualFunctions =
      Set.toUnfoldable
        $ Map.keys
        $ Map.filter (is _VisualFunction)
        $ view _functions state

    state' =
      foldr
        ( \functionName state'' ->
            fromMaybe state'' do
              functionType <- Map.lookup (Location functionName) typeMap
              let
                inputDocs =
                  List.toUnfoldable
                    $ List.mapWithIndex (\index -> const { name: "Input " <> show index, description: "An input for a custom function" })
                    $ inputs functionType

                functionData = internal inputDocs { name: show functionName <> " output", description: "The output of a custom function" }
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

-- Removes the partial connection. Here to solve #40
clearPartialConnection :: forall a s m. State a s m -> State a s m
clearPartialConnection = set _partialFrom Nothing <<< set _partialTo Nothing <<< set _unconnectablePins mempty

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
  flip execState state do
    let
      id = NodeId $ show name <> "-output"

      function = createFunction name id
    modify_ $ over _project function
    modify_ $ setCurrentFunction (Just name)
    modify_ $ set _currentGeometryCache $ Just def
    modify_ $ set (_atFunctionData name) (Just def)
    modify_ compile

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

-- Counts how many times a function is used inside another function
countFunctionRefs :: FunctionName -> G.Graph NodeId Node -> Int
countFunctionRefs name = G.size <<< G.filterVertices ((_ == name) <<< getFunctionName)

-- Deletes a node form a given function
deleteNode :: forall a s m. FunctionName -> NodeId -> State a s m -> State a s m
deleteNode functionName id state =
  flip execState state
    $ when (not isOutput) do
        let
          nodes = preview (_nodes functionName) state

          -- The function the node runs
          nodeFunction = fromMaybe (FunctionName "") $ getFunctionName <$> node

          -- If this is the last reference to the used function in the current function we remove the edge from the dependency graph
          functionRefCount = countFunctionRefs nodeFunction (fromMaybe emptyGraph $ preview (_nodes functionName) state)
        modify_
          $ over (_nodes functionName)
          $ map
          $ over _nodeInputs
          $ map \input -> if input == Just id then Nothing else input
        modify_ $ over (_nodes functionName) $ G.delete id
        -- TODO: make this work with the new foreign system
        -- modify_ $ set (_atNodeData functionName id) Nothing
        modify_ $ over (_currentNodeGroup <<< _Just <<< _NodeGroupInputs) $ filter (id /= _)
        modify_ compile
  where
  node = join $ preview (_atNode functionName id) state

  isOutput = maybe false (is _OutputNode) node

-- Delete all the nodes runnign a certain functions inside another functions
deleteFunctionReferences :: forall a s m. FunctionName -> FunctionName -> G.Graph NodeId Node -> State a s m -> State a s m
deleteFunctionReferences toDelete functionName graph state =
  foldr (deleteNode functionName) state
    $ filterMap
        ( \(Tuple nodeId node) ->
            if getFunctionName node == toDelete then
              Just nodeId
            else
              Nothing
        )
    $ (G.toUnfoldable graph :: List _)

-- Delete a function from the state
deleteFunction :: forall a s m. FunctionName -> State a s m -> State a s m
deleteFunction toDelete state =
  flip execState state do
    let
      visualFunctions =
        filterMap
          ( \(Tuple name function) ->
              Tuple name
                <$> preview _VisualFunction function
          )
          $ ( Map.toUnfoldable
                $ view _functions state ::
                List _
            )
    put
      $ foldr
          (\(Tuple functionName nodeGroup) -> deleteFunctionReferences toDelete functionName $ view _NodeGroupNodes nodeGroup)
          state
          visualFunctions
    modify_ $ set (_atFunctionData toDelete) Nothing
    modify_ $ set (_function toDelete) Nothing
    -- Todo: make this work with the new foreign system
    -- modify_ $ over _nodeData $ Map.filterKeys $ (_ /= toDelete) <<< fst
    modify_ $ over _runtimeOverwrites $ Newtype.over ValueMap $ Map.filterKeys $ (_ /= Just toDelete) <<< preview _ExtendedLocation
    modify_ $ set (_atInputCount toDelete) Nothing
    when (view _currentFunction state == Just toDelete) $ modify_ $ set _currentFunction Nothing
    modify_ compile

-- Sets the runtime value at a location to any runtime value
setRuntimeValue :: forall a s m. FunctionName -> NodeId -> RuntimeValue -> State a s m -> State a s m
setRuntimeValue functionName nodeId value =
  evaluate
    <<< set
        (_runtimeOverwrites <<< newtypeIso <<< at (DeepLocation functionName $ Location nodeId))
        (Just value)

visualFunctionCount :: forall a s m. State a s m -> Int
visualFunctionCount = Map.size <<< filter (is _VisualFunction) <<< view _functions

-- Get the number of nodes in a state
nodeCount :: forall a s m. State a s m -> Int
nodeCount = unwrap <<< foldMap (Additive <<< go) <<< view _functions
  where
  go (VisualFunction (NodeGroup { nodes })) = length nodes

  go _ = 0

-- Search using the current search term in the state
searchNode :: forall a s m. State a s m -> Array FunctionName
searchNode state = sortBySearch show searchTerm nodes
  where
  searchTerm = view _nodeSearchTerm state

  nodes = Set.toUnfoldable $ Map.keys $ view _functionData state

-- Check if a function exists
functionExists :: forall a s m. FunctionName -> State a s m -> Boolean
functionExists name = Map.member name <<< view _functionData

-- Prevent and stop the propagation for any dom event
preventDefaults :: forall q i o m a s. MonadEffect m => Event -> HalogenM (State a s m) q i o m Unit
preventDefaults event = liftEffect $ Event.preventDefault event *> Event.stopPropagation event

-- Lenses
_inputCountMap :: forall a s m. Lens' (State a s m) (Map FunctionName Int)
_inputCountMap = prop (SProxy :: _ "inputCountMap")

_atInputCount :: forall a s m. FunctionName -> Traversal' (State a s m) (Maybe Int)
_atInputCount name = _inputCountMap <<< at name

_runtimeOverwrites :: forall a s m. Lens' (State a s m) (ValueMap Location)
_runtimeOverwrites = prop (SProxy :: _ "runtimeOverwrites")

_valueMap :: forall a s m. Lens' (State a s m) (ValueMap Location)
_valueMap = prop (SProxy :: _ "valueMap")

_geometries :: forall a s m. Lens' (State a s m) (Map FunctionName GeometryCache)
_geometries = prop (SProxy :: _ "geometries")

_atGeometry :: forall a s m. FunctionName -> Lens' (State a s m) (Maybe GeometryCache)
_atGeometry name = _geometries <<< at name

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

_expression :: forall a s m. Lens' (State a s m) (Expression Location)
_expression = prop (SProxy :: _ "expression")

_typeMap :: forall a s m. Lens' (State a s m) (Map Location Type)
_typeMap = prop (SProxy :: _ "typeMap")

_nextId :: forall a s m. Lens' (State a s m) Int
_nextId = prop (SProxy :: _ "nextId")

_functions :: forall a s m. Lens' (State a s m) (Map.Map FunctionName DataflowFunction)
_functions = _project <<< _ProjectFunctions

_nodeGroup :: forall a s m. FunctionName -> Traversal' (State a s m) NodeGroup
_nodeGroup name = _project <<< _projectNodeGroup name

_nodes :: forall a s m. FunctionName -> Traversal' (State a s m) (G.Graph NodeId Node)
_nodes name = _nodeGroup name <<< _NodeGroupNodes

_atNode :: forall a s m. FunctionName -> NodeId -> Traversal' (State a s m) (Maybe Node)
_atNode name id = _project <<< _atProjectNode name id

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

_currentGeometryCache :: forall a s m. Lens' (State a s m) (Maybe GeometryCache)
_currentGeometryCache =
  ( lens
      ( \state -> do
          currentFunction <- view _currentFunction state
          view (_atGeometry currentFunction) state
      )
      ( \state maybeValue ->
          fromMaybe state do
            currentFunction <- view _currentFunction state
            pure $ set (_atGeometry currentFunction) maybeValue state
      )
  )

_currentNodes :: forall a s m. Traversal' (State a s m) (G.Graph NodeId Node)
_currentNodes = _currentNodeGroup <<< _Just <<< _NodeGroupNodes

_atCurrentNode :: forall a s m. NodeId -> Traversal' (State a s m) Node
_atCurrentNode id = _currentNodes <<< ix id

_functionUis :: forall a s m. Lens' (State a s m) (Map FunctionName (FunctionUi a s m))
_functionUis = prop (SProxy :: _ "functionUis")

_ui :: forall a s m. FunctionName -> Traversal' (State a s m) (Maybe (FunctionUi a s m))
_ui functionName = _functionUis <<< at functionName

_unconnectablePins :: forall a s m. Lens' (State a s m) (Set.Set (ExtendedLocation NodeId Pin))
_unconnectablePins = prop (SProxy :: _ "unconnectablePins")

_name :: forall a s m. Lens' (State a s m) String
_name = prop (SProxy :: _ "name")

_isExample :: forall a s m. Lens' (State a s m) Boolean
_isExample = prop (SProxy :: _ "isExample")

_isAdmin :: forall a s m. Lens' (State a s m) Boolean
_isAdmin = prop (SProxy :: _ "isAdmin")

_isVisible :: forall a s m. Lens' (State a s m) Boolean
_isVisible = prop (SProxy :: _ "isVisible")

_nodeSearchTerm :: forall a s m. Lens' (State a s m) String
_nodeSearchTerm = prop (SProxy :: _ "nodeSearchTerm")
