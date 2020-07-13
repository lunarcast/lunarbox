module Lunarbox.Data.Editor.State where

import Prelude
import Color (cssStringRGBA)
import Control.Monad.State (class MonadState, execState, execStateT, get, gets, put, runState)
import Control.Monad.State as StateM
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Default (def)
import Data.Filterable (filter, filterMap)
import Data.Foldable (foldMap, foldr, length, traverse_)
import Data.Lens (Lens', Traversal', _Just, is, lens, over, preview, set, traversed, view)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
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
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Unfoldable (replicate)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM, liftEffect, modify_)
import Lunarbox.Capability.Editor.Type (generateColorMap, inputNodeType, typeToColor)
import Lunarbox.Control.Monad.Dataflow.Interpreter (InterpreterContext(..), evalInterpreter, runInterpreter)
import Lunarbox.Control.Monad.Dataflow.Interpreter.Interpret (interpret, normalizeTerm)
import Lunarbox.Control.Monad.Dataflow.Solve (SolveState(..))
import Lunarbox.Control.Monad.Dataflow.Solve.SolveExpression (solveExpression)
import Lunarbox.Control.Monad.Dataflow.Solve.Unify (canUnify)
import Lunarbox.Data.Class.GraphRep (toGraph)
import Lunarbox.Data.Dataflow.Expression (Expression(..))
import Lunarbox.Data.Dataflow.Expression.Lint (LintError, lint)
import Lunarbox.Data.Dataflow.Expression.Optimize (dce, inline)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.Runtime.TermEnvironment (Term(..))
import Lunarbox.Data.Dataflow.Runtime.ValueMap (ValueMap(..))
import Lunarbox.Data.Dataflow.Type (Type(..), inputs)
import Lunarbox.Data.Dataflow.TypeError (TypeError)
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction(..), _VisualFunction)
import Lunarbox.Data.Editor.FunctionData (FunctionData(..), _FunctionDataInputs, internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.FunctionUi (FunctionUi)
import Lunarbox.Data.Editor.Location (Location(..), _Function)
import Lunarbox.Data.Editor.Node (Node(..), _OutputNode, _nodeInput, _nodeInputs, getFunctionName)
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..), ScopedLocation(..))
import Lunarbox.Data.Editor.NodeGroup (NodeGroup(..), _NodeGroupInputs, _NodeGroupNodes, _NodeGroupOutput)
import Lunarbox.Data.Editor.Project (Project(..), _ProjectFunctions, _ProjectMain, _atProjectFunction, _atProjectNode, _projectNodeGroup, compileProject, createFunction)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Ord (sortBySearch)
import Lunarbox.Foreign.Render (GeometryCache, ForeignTypeMap, emptyGeometryCache)
import Lunarbox.Foreign.Render as Native
import Record as Record
import Type.Row (type (+))
import Web.Event.Event as Event
import Web.Event.Internal.Types (Event)

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

-- | The result of compiling and typechecking the program
type CompilationResult r
  = ( expression :: Expression Location
    , typeMap :: Map Location Type
    , typeErrors :: Array (TypeError Location)
    , lintingErrors :: Array (LintError Location)
    | r
    )

type StatePermanentData r
  = ( project :: Project
    , nextId :: Int
    , geometries :: Map FunctionName GeometryCache
    , runtimeOverwrites :: ValueMap Location
    , currentFunction :: FunctionName
    | r
    )

-- | The state of the entire editor
type State a s m
  = { 
    | StatePermanentData
      + CompilationResult
      + ( currentTab :: Tab
      , functionData :: Map FunctionName FunctionData
      , panelIsOpen :: Boolean
      , valueMap :: ValueMap Location
      , functionUis :: Map FunctionName (FunctionUi a s m)
      , inputCountMap :: Map FunctionName Int
      , pendingConnection ::
        Maybe
          { from :: NodeId
          , toId :: NodeId
          , toIndex :: Int
          , compilationResult :: { | CompilationResult () }
          }
      , name :: String
      , isExample :: Boolean
      , isAdmin :: Boolean
      , nodeSearchTerm :: String
      , isVisible :: Boolean
      , currentlyEditedNode :: Maybe NodeId
      )
    }

-- Starting state which contains nothing
emptyState :: forall a s m f. MonadEffect f => f (State a s m)
emptyState =
  initializeFunction (FunctionName "main")
    { currentTab: Settings
    , currentFunction: FunctionName "main"
    , nextId: 0
    , panelIsOpen: false
    , typeMap: mempty
    , functionData: mempty
    , valueMap: mempty
    , runtimeOverwrites: mempty
    , functionUis: mempty
    , inputCountMap: mempty
    , geometries: mempty
    , expression: TypedHole UnknownLocation
    , project: Project { main: FunctionName "main", functions: mempty }
    , name: "Unnamed project"
    , nodeSearchTerm: ""
    , isExample: false
    , isAdmin: false
    , isVisible: false
    , pendingConnection: Nothing
    , typeErrors: []
    , lintingErrors: []
    , currentlyEditedNode: Nothing
    }

-- Helpers
-- Generate an id and icncrease the inner counter in the state
createId :: forall a s m. StateM.State (State a s m) NodeId
createId = do
  nextId <- gets $ view _nextId
  modify_ $ over _nextId (_ + 1)
  pure $ NodeId $ show nextId

-- TODO: make it so you don't have to pass exactly this name to createNode to create an input node
inputNodeName :: FunctionName
inputNodeName = FunctionName "input"

-- 
-- Update the way a node looks to fit the latest data
updateNode :: forall m a s n. MonadState (State a s n) m => MonadEffect m => NodeId -> m Unit
updateNode id =
  gets (view _currentGeometryCache)
    >>= traverse_ \cache ->
        withCurrentFunction \currentFunction ->
          gets (preview $ _atCurrentNode id)
            >>= traverse_ \node -> do
                { typeMap, valueMap } <- get
                nodeGroup <- gets $ preview (_nodeGroup currentFunction)
                let
                  value =
                    Nullable.toNullable
                      $ (show <<< evalInterpreter def <<< normalizeTerm)
                      <$> getNodeValue currentFunction valueMap id node

                  inputs = List.toUnfoldable $ view _nodeInputs node

                  colorMap = case node of
                    InputNode ->
                      fromMaybe mempty do
                        group <- nodeGroup
                        ty <- Map.lookup (AtFunction currentFunction) typeMap
                        pure $ typeToColor <$> inputNodeType group id ty
                    _ -> generateColorMap (\pin -> flip Map.lookup typeMap $ InsideFunction currentFunction $ PinLocation id pin) node
                -- let
                -- (ValueMap v) = valueMap
                -- 
                -- (m :: Array _) = Map.toUnfoldable v
                -- trace m \_ -> pure unit
                liftEffect
                  $ Native.refreshInputArcs cache id
                      { inputs: Nullable.toNullable <$> inputs
                      , value
                      , colorMap:
                        foldr (uncurry go)
                          { output: Nullable.null
                          , inputs: replicate (length inputs) Nullable.null
                          }
                          (Map.toUnfoldable colorMap :: Array _)
                      }
  where
  go OutputPin color map = map { output = Nullable.notNull $ cssStringRGBA color }

  go (InputPin index) color map =
    map
      { inputs =
        fromMaybe map.inputs
          $ Array.updateAt index (Nullable.notNull $ cssStringRGBA color) map.inputs
      }

--
-- Create a new node and save all the data in the different required places
createNode :: forall m a s n. MonadEffect m => MonadState (State a s n) m => FunctionName -> m Unit
createNode name = do
  let
    isInput = name == inputNodeName

    create = do
      state <- get
      id <- createId
      desiredInputCount <- gets $ preview $ _atInputCount name
      let
        maxInputs =
          if isInput then
            0
          else
            getMaxInputs name state

        inputCount = fromMaybe maxInputs $ join desiredInputCount

        node =
          if isInput then
            InputNode
          else
            ComplexNode
              { inputs: replicate inputCount Nothing
              , function: name
              }
      withCurrentFunction_ \currentFunction -> do
        modify_ $ set (_atNode currentFunction id) node
        when isInput do
          modify_
            $ over (_atFunctionData currentFunction <<< _Just <<< _FunctionDataInputs)
                (_ <> [ { name: "input", description: "The input of a custom function" } ])
          modify_ $ over (_currentNodeGroup <<< _Just <<< _NodeGroupInputs) $ (_ <> pure id)
        modify_ compile
      pure $ Tuple id inputCount
  Tuple (Tuple id inputs) newState <- gets $ runState create
  gets (view _currentGeometryCache)
    >>= traverse_ \cache -> do
        let
          displayName = if isInput then Nullable.null else Nullable.notNull $ show name
        liftEffect $ Native.createNode cache id inputs true displayName
        void $ put newState
        updateNode id

-- Get the type of the output a node (Also works for input pins)
getOutputType :: forall a s m. FunctionName -> NodeId -> State a s m -> Maybe Type
getOutputType functionName id state = do
  let
    typeMap = view _typeMap state
  nodeGroup <- preview (_nodeGroup functionName) state
  currentFunctionType <- Map.lookup (AtFunction functionName) typeMap
  let
    inputIndex = List.findIndex (_ == id) $ view _NodeGroupInputs nodeGroup
  case inputIndex of
    Just index -> (inputs currentFunctionType) `List.index` index
    Nothing -> Map.lookup (InsideFunction functionName $ PinLocation id OutputPin) typeMap

-- Generate the list of inputs can't be connected 
generateUnconnectableInputs :: forall a s m. NodeId -> State a s m -> Set.Set { id :: NodeId, index :: Int }
generateUnconnectableInputs output state = Set.map (\(Tuple id index) -> { id, index }) unconnectableInputs
  where
  unconnectableInputs = Set.filter (not <<< flip (canConnect output) state) allInputs

  allInputs =
    Set.fromFoldable
      $ ( \(Tuple id node) ->
            List.mapWithIndex (const <<< Tuple id) $ view _nodeInputs node
        )
      =<< nodeGroup

  nodeGroup :: List _
  nodeGroup = maybe mempty Map.toUnfoldable $ preview _currentNodes state

-- Generates a list of outputs which can't be connected
generateUnconnectableOutputs :: forall a s m. Tuple NodeId Int -> State a s m -> Set.Set NodeId
generateUnconnectableOutputs input state = Set.filter (\outputId -> not $ canConnect outputId input state') outputs
  where
  state' = removeConnection input state

  keys = maybe mempty Map.keys $ preview _currentNodes state'

  outputNode = preview (_currentNodeGroup <<< _Just <<< _NodeGroupOutput) state'

  outputs = case outputNode of
    Just id -> Set.difference keys $ Set.singleton id
    Nothing -> keys

-- Generate the next expression / typeMap of a project
tryCompiling ::
  forall a s m.
  State a s m ->
  { | CompilationResult () }
tryCompiling state@{ project } = result
  where
  expression' = compileProject project

  result =
    { typeErrors: errors
    , lintingErrors: lintingErrors
    , typeMap: typeMap
    , expression: inline $ dce expression'
    }
    where
    lintingErrors = lint expression'

    (Tuple typeMap (SolveState { errors })) = solveExpression expression'

-- Compile a project
compile :: forall a s m. State a s m -> State a s m
compile state =
  evaluate
    $ Record.merge overwrites state
  where
  overwrites = tryCompiling state

-- Evaluate the current expression and write into the value map
evaluate :: forall a s m. State a s m -> State a s m
evaluate state = set _valueMap valueMap state
  where
  context =
    InterpreterContext
      { location: UnknownLocation
      , termEnv: mempty
      , overwrites: view _runtimeOverwrites state
      , toplevel: true
      }

  expression = view _expression state

  valueMap =
    snd
      $ runInterpreter
          context
      $ interpret expression

-- Check if 2 pins can be connected
canConnect :: forall a s m. NodeId -> Tuple NodeId Int -> State a s m -> Boolean
canConnect from (Tuple toId toIndex) state =
  fromMaybe false do
    let
      typeMap = view _typeMap state

      currentFunction = view _currentFunction state
    nodes <- preview _currentNodes state
    guard $ not $ G.wouldCreateCycle from toId $ toGraph nodes
    fromType <- getOutputType currentFunction from state
    toType <- Map.lookup (InsideFunction currentFunction $ PinLocation toId $ InputPin toIndex) typeMap
    guard $ canUnify toType fromType
    pure true

-- Creates a connection from a node id to a node id an an input index
createConnection :: forall a s m. NodeId -> NodeId -> Int -> State a s m -> State a s m
createConnection from toId toIndex =
  set
    ( _atCurrentNode toId
        <<< _nodeInput toIndex
    )
    (Just from)

-- Set the function the user is editing at the moment
setCurrentFunction :: forall a s m. FunctionName -> State a s m -> State a s m
setCurrentFunction = set _currentFunction

-- Creates a function, adds an output node and set it as the current edited function
initializeFunction :: forall a s m monad. MonadEffect monad => FunctionName -> State a s m -> monad (State a s m)
initializeFunction name state =
  flip execStateT state do
    let
      id = NodeId $ show name <> "-output"
    cache <- liftEffect emptyGeometryCache
    liftEffect $ Native.createNode cache id 1 false Nullable.null
    modify_ $ over _project $ createFunction name id
    modify_ $ setCurrentFunction name
    modify_ $ set _currentGeometryCache $ Just cache
    modify_ $ set (_atFunctionData name) $ Just $ internal [] { name: show name <> "-output", description: "The output of a custom functions" }
    modify_ compile

-- Remove a conenction from the current function
removeConnection :: forall a s m. Tuple NodeId Int -> State a s m -> State a s m
removeConnection (Tuple toId toIndex) =
  execState
    $ withCurrentFunction_ \currentFunction -> do
        modify_ $ set (_atNode currentFunction toId <<< _nodeInput toIndex) Nothing
        modify_ compile

-- Deletes a node form a given function
deleteNode :: forall a s m. FunctionName -> NodeId -> State a s m -> State a s m
deleteNode functionName id state =
  flip execState state
    $ when (not isOutput) do
        modify_
          $ over (_nodes functionName <<< traversed <<< _nodeInputs <<< traversed) \input -> if input == Just id then Nothing else input
        modify_ $ over (_currentNodeGroup <<< _Just <<< _NodeGroupInputs) $ filter (id /= _)
        modify_ $ over (_currentNodeGroup <<< _Just <<< _NodeGroupNodes)
          $ Map.filterKeys (id /= _)
        modify_ compile
  where
  node = preview (_atNode functionName id) state

  isOutput = maybe false (is _OutputNode) node

-- Delete all the nodes runnign a certain functions inside another functions
deleteFunctionReferences :: forall a s m. FunctionName -> FunctionName -> Map NodeId Node -> State a s m -> State a s m
deleteFunctionReferences toDelete functionName graph state =
  foldr (deleteNode functionName) state
    $ filterMap
        ( \(Tuple nodeId node) ->
            if getFunctionName node == toDelete then
              Just nodeId
            else
              Nothing
        )
    $ (Map.toUnfoldable graph :: List _)

-- Delete a function from the state
deleteFunction :: forall a s m. FunctionName -> State a s m -> State a s m
deleteFunction toDelete state =
  flip execState state
    $ unless (main == toDelete) do
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
        modify_ $ over _runtimeOverwrites $ Newtype.over ValueMap $ Map.filterKeys $ (_ /= Just toDelete) <<< view _Function
        modify_ $ set (_atInputCount toDelete) Nothing
        modify_ $ set (_atGeometry toDelete) Nothing
        when (view _currentFunction state == toDelete) $ modify_ $ set _currentFunction main
        modify_ compile
  where
  main = view _ProjectMain state.project

-- Sets the runtime value at a location to any runtime value
setRuntimeValue :: forall a s m. FunctionName -> NodeId -> RuntimeValue -> State a s m -> State a s m
setRuntimeValue functionName nodeId value =
  evaluate
    <<< set
        (_runtimeOverwrites <<< _Newtype <<< at (InsideFunction functionName $ NodeLocation nodeId))
        (Just $ Term value)

-- Count the number of visual user-defined functions in a project
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

-- | Run an action which needs access to the current function
withCurrentFunction :: forall f a s m r. MonadState (State a s m) f => (FunctionName -> f r) -> f r
withCurrentFunction f = gets (view _currentFunction) >>= f

-- | Run an action which need access to the current function and which returns nothing
withCurrentFunction_ :: forall f a s m r. MonadState (State a s m) f => (FunctionName -> f r) -> f Unit
withCurrentFunction_ = void <<< withCurrentFunction

-- | Run an action which needs access to the current geometry cache
withCurrentGeometries :: forall f a s m r. MonadState (State a s m) f => (GeometryCache -> f r) -> f (Maybe r)
withCurrentGeometries f = gets (view _currentGeometryCache) >>= traverse f

-- | Run an action which needs access to the id of the node the user is curently editing
withCurrentNode_ :: forall f a s m r. MonadState (State a s m) f => (NodeId -> f r) -> f Unit
withCurrentNode_ f = void $ gets (_.currentlyEditedNode) >>= traverse f

-- | Get the maximum number of inputs a node can take
getMaxInputs ::
  forall r.
  FunctionName ->
  Record
    ( functionData :: Map.Map FunctionName FunctionData
    , project :: Project
    | r
    ) ->
  Int
getMaxInputs name { project: Project { functions }, functionData } = case Map.lookup name functionData of
  Just (FunctionData { inputs }) -> Array.length inputs
  Nothing -> case Map.lookup name functions of
    Just (VisualFunction (NodeGroup { inputs })) -> List.length inputs
    _ -> 0

-- | Get the value to be displayed underneath a node
getNodeValue :: FunctionName -> ValueMap Location -> NodeId -> Node -> Maybe (Term Location)
getNodeValue currentFunction (ValueMap vmap) id = case _ of
  ComplexNode _ -> Map.lookup (InsideFunction currentFunction $ NodeLocation id) vmap
  OutputNode _ -> Map.lookup (InsideFunction currentFunction $ PinLocation id $ InputPin 0) vmap
  _ -> Nothing

-- Update all nodes in the current geometry
updateAll :: forall a s m n. MonadState (State a s n) m => MonadEffect m => m (Maybe GeometryCache)
updateAll =
  withCurrentFunction \name -> do
    cache <- gets $ view $ _atGeometry name
    (map (maybe mempty Map.keys) $ gets $ preview $ _nodes name)
      >>= traverse_ updateNode
    pure cache

getFunctionColorMap :: Int -> Type -> ForeignTypeMap
getFunctionColorMap 0 ty = { inputs: [], output: Nullable.notNull $ cssStringRGBA $ typeToColor ty }

getFunctionColorMap n (TConstant "Function" [ from, to ]) =
  next
    { inputs =
      [ Nullable.notNull (cssStringRGBA $ typeToColor from) ]
        <> next.inputs
    }
  where
  next = getFunctionColorMap (n - 1) to

getFunctionColorMap n ty = getFunctionColorMap 0 ty

-- | Possible steps we need to take to focus on something
data MovementStep
  = MoveToFunction FunctionName
  | CenterNode NodeId
  | CenterOutput

-- | Get the steps for moving towards a location 
moveTo :: Location -> Array MovementStep
moveTo UnknownLocation = []

moveTo (AtFunction name) = [ MoveToFunction name ]

moveTo (AtFunctionDeclaration name) = [ MoveToFunction name ]

moveTo (FixpointOperator name) = [ MoveToFunction name ]

moveTo (InsideFunction name deep) = [ MoveToFunction name ] <> go deep
  where
  go (NodeLocation id) = [ CenterNode id ]

  go (NodeDefinition id) = [ CenterNode id ]

  go (PinLocation id _) = [ CenterNode id ]

  go (AtApplication id _) = [ CenterNode id ]

  go FunctionDeclaration = [ CenterOutput ]

  go _ = []

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

_atFunctionData :: forall a s m. FunctionName -> Traversal' (State a s m) (Maybe FunctionData)
_atFunctionData name = _functionData <<< at name

_project :: forall a s m. Lens' (State a s m) Project
_project = prop (SProxy :: _ "project")

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

_nodes :: forall a s m. FunctionName -> Traversal' (State a s m) (Map NodeId Node)
_nodes name = _nodeGroup name <<< _NodeGroupNodes

_atNode :: forall a s m. FunctionName -> NodeId -> Traversal' (State a s m) Node
_atNode name id = _project <<< _atProjectNode name id <<< _Just

_function :: forall a s m. FunctionName -> Traversal' (State a s m) (Maybe DataflowFunction)
_function name = _project <<< _atProjectFunction name

_currentFunction :: forall a s m. Lens' (State a s m) FunctionName
_currentFunction = prop (SProxy :: _ "currentFunction")

_panelIsOpen :: forall a s m. Lens' (State a s m) Boolean
_panelIsOpen = prop (SProxy :: _ "panelIsOpen")

_currentTab :: forall a s m. Lens' (State a s m) Tab
_currentTab = prop (SProxy :: _ "currentTab")

_currentNodeGroup :: forall a s m. Lens' (State a s m) (Maybe NodeGroup)
_currentNodeGroup =
  ( lens
      ( \state ->
          let
            current = view _currentFunction state
          in
            preview (_nodeGroup current) state
      )
      ( \state maybeValue ->
          fromMaybe state do
            let
              currentFunction = view _currentFunction state
            value <- maybeValue
            pure $ set (_nodeGroup currentFunction) value state
      )
  )

_currentGeometryCache :: forall a s m. Lens' (State a s m) (Maybe GeometryCache)
_currentGeometryCache =
  ( lens
      ( \state ->
          let
            currentFunction = view _currentFunction state
          in
            view (_atGeometry currentFunction) state
      )
      ( \state maybeValue ->
          let
            currentFunction = view _currentFunction state
          in
            set (_atGeometry currentFunction) maybeValue state
      )
  )

_currentNodes :: forall a s m. Traversal' (State a s m) (Map NodeId Node)
_currentNodes = _currentNodeGroup <<< _Just <<< _NodeGroupNodes

_atCurrentNode :: forall a s m. NodeId -> Traversal' (State a s m) Node
_atCurrentNode id = _currentNodes <<< ix id

_functionUis :: forall a s m. Lens' (State a s m) (Map FunctionName (FunctionUi a s m))
_functionUis = prop (SProxy :: _ "functionUis")

_ui :: forall a s m. FunctionName -> Traversal' (State a s m) (Maybe (FunctionUi a s m))
_ui functionName = _functionUis <<< at functionName

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
